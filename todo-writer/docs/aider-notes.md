# Running the doc pipeline on Aider + free-tier models

Notes from setting up Aider as an alternative client for `fill-doc-todos.sh`, so
the undocumented-declaration PRs can be driven by free-tier LLM services instead
of Claude Code / Codex subscriptions. Written 2026-07-31, before the first
container rebuild, so the install section is unverified.

## 1. Devcontainer changes (already committed)

Committed to the devcontainer repo on `dc-template` ("Add Aider to the
devcontainer") and merged into `dc-scala`. Run `sync-devcontainers.sh` to
propagate to the other `dc-*` branches.

`.devcontainer/Dockerfile`:

```dockerfile
ARG AIDER_VERSION=latest
...
RUN mkdir -p /workspace /home/node/.claude /home/node/.codex /home/node/.aider && \
  chown -R node:node /workspace /home/node/.claude /home/node/.codex /home/node/.aider
...
ENV PATH=$PATH:/usr/local/share/npm-global/bin:/home/node/.local/bin
...
RUN curl -LsSf https://astral.sh/uv/install.sh | sh \
  && uv tool install --force --python python3.12 --with pip aider-chat@${AIDER_VERSION}
```

`.devcontainer/devcontainer.json`: `AIDER_VERSION` build arg, an
`aider-config-${devcontainerId}` volume mounted at `/home/node/.aider`, and
`AIDER_CHECK_UPDATE=false` in `containerEnv`.

Two reasons it is done this way:

- **Build time, not runtime.** The firewall (below) blocks PyPI, so
  `pip install aider-chat` inside a running container cannot work. It has to be
  baked into the image.
- **uv, not system pip.** Debian 12 marks system Python as externally managed
  (PEP 668), so `pip install --user` is refused. uv also fetches its own Python
  3.12, which keeps aider off the system `python3` that sbt and friends see.

A container rebuild is required before `aider` exists.

## 2. The firewall is the real gate

`.devcontainer/init-firewall.sh` resolves an allowlist of domains at container
start, pins the resulting IPs in an ipset, and REJECTs all other outbound
traffic. Measured from inside the container before the rebuild:

```
pypi.org                                  BLOCKED
astral.sh                                 BLOCKED
openrouter.ai                             BLOCKED
generativelanguage.googleapis.com         BLOCKED
api.anthropic.com                         reachable
```

The allowlist is `api.anthropic.com`, `api.openai.com`, `auth.openai.com`,
`chatgpt.com`, plus GitHub, npm, Sentry/Statsig, VS Code marketplace and Maven
Central. **Every free-tier provider is blocked until its host is added.**

To add one, put the hostname in the `for domain in \` loop in
`init-firewall.sh`, then restart the container (or re-run
`sudo /usr/local/bin/init-firewall.sh`):

```bash
for domain in \
    "registry.npmjs.org" \
    "api.anthropic.com" \
    "openrouter.ai" \
    ...
```

(Do not put a trailing `# comment` on those lines; the list uses backslash
continuations and a comment after the backslash breaks the loop.)

Caveat worth remembering when choosing a provider: the ipset is resolved **once**
at container start. Providers behind a small, stable set of IPs (OpenRouter,
Groq) work fine. `generativelanguage.googleapis.com` (Google AI Studio) rotates
across a large pool, so a session that starts fine will begin failing partway
through as DNS hands out addresses that were never added to the ipset. Prefer
providers with stable IPs, or the firewall needs a different approach for that
host.

## 3. Using Aider

### Interactive

```bash
aider --model <model> library/src/scala/io/Source.scala
```

Then type instructions at the prompt. Useful in-session commands: `/ask`
(question without editing), `/diff`, `/undo`, `/drop`, `/run <cmd>`, `/tokens`.

### Scripted (the mode this pipeline needs)

```bash
aider --model "$WRITER_MODEL" \
      --message-file prompt.txt \
      --yes-always --no-auto-commits --no-gitignore \
      --map-tokens 0 --no-stream --no-check-update \
      --read todo-writer/docs/house-rules.md \
      "$FILE"
```

`--message-file` runs one shot and exits, which is the same shape as
`claude -p` / `codex exec`.

### Flags that matter here specifically

| Flag | Why |
|---|---|
| `--no-auto-commits` | **Non-negotiable.** Aider commits every edit by default. The whole `run-missing-doc-todos.sh` design (generate broadly, revert outside the keep scope, review the working tree, commit one PR by hand) assumes changes sit uncommitted. Without this it will commit into the branch mid-run. |
| `--yes-always` | No interactive confirmations, equivalent to `--dangerously-skip-permissions`. |
| `--no-gitignore` | Stops aider appending `.aider*` entries to scala3's `.gitignore`, which is an upstream file that must not be touched. |
| `--map-tokens 0` | Disables the repo map. On a repo the size of scala3 the map is enormous and would dominate token spend for no benefit, since each call targets exactly one file. |
| `--read <file>` | Adds a file as read-only context. Cleaner fit for `docs/house-rules.md` than the current `house_rules()` prompt concatenation. |
| `--no-stream` | Keeps stdout parseable. |
| `--env-file /home/node/.aider/.env` | Where to keep the API key. Only `/home/node/.aider` is a persisted volume; a container rebuild wipes the rest of the home directory, so a key in `~/.aider.conf.yml` or `~/.zshrc` is lost on every rebuild. |
| `--chat-mode ask` | Read-only mode, will not edit files. |

### Model names

Aider routes through LiteLLM, so names are provider-prefixed:

```
openrouter/deepseek/deepseek-chat
gemini/gemini-2.5-flash
groq/llama-3.3-70b-versatile
```

API keys come from the matching env var (`OPENROUTER_API_KEY`, `GEMINI_API_KEY`,
`GROQ_API_KEY`) or `--api-key provider=key`.

## 4. How to wire it into fill-doc-todos.sh

Recommendation: **use Aider only for the writer and refine steps, and plain
`curl` for the two reviewer steps.**

The current loop has three roles:

| Role | Today | On Aider |
|---|---|---|
| Writer / refine | `claude -p --model opus --allowedTools Edit,Read,Grep,Glob` | Aider edit mode, file named on the command line. Good fit. |
| Accuracy review | `codex exec --output-schema $SCHEMA` | Not Aider. See below. |
| Style review | `claude -p --model sonnet --output-format json` | Not Aider. See below. |

The reviewers should skip Aider because:

- Both prompts already inline everything they need. `render` substitutes the
  file path and `DIFF_BLOCK` supplies the unified diff, and the reviewers only
  ever had `Read,Grep,Glob`. There is no editing to do, so an editing client
  buys nothing.
- Aider has no `--output-format json` and no output-schema support equivalent to
  `codex exec --output-schema`. Its stdout also carries banner, model and cost
  lines that `clean_json()` would have to fight.
- A direct POST to the provider's OpenAI-compatible `/chat/completions` is
  cheaper, and most free tiers support `response_format: {"type":"json_object"}`,
  which gets back clean JSON matching `schemas/doc-review.schema.json` without
  the fence-and-preamble stripping.

Rule of thumb: Aider for the steps that edit files, curl for the steps that only
judge.

## 5. Rebuild checklist

1. Pick the provider, add its host to `init-firewall.sh`, commit on
   `dc-template`.
2. Rebuild the container.
3. `aider --version` to confirm the install worked.
4. `curl -s -o /dev/null -w "%{http_code}" https://<provider-host>` to confirm
   the firewall change took.
5. Put the API key in `/home/node/.aider/.env` (persisted volume).
6. Smoke test on one small file, checking that **nothing was committed**:

```bash
git checkout -- library/src/scala/io/
todo-writer/scripts/run-missing-doc-todos.sh --mark-only io-ref
aider --model <model> --message "Fill in the TODO FILL IN Scaladoc." \
      --yes-always --no-auto-commits --no-gitignore --map-tokens 0 \
      library/src/scala/io/Source.scala
git log --oneline -1   # must be unchanged
git diff --stat        # changes must be here, uncommitted
```
