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
inference.poolside.ai                     BLOCKED (now allowlisted, needs rebuild)
api.anthropic.com                         reachable
```

**Every provider is blocked until its host is added.** This also applies to the
`WebFetch` tool in Claude Code, which runs from inside the container: it can
fetch github.com but not `docs.poolside.ai`, which is worth knowing before
trusting it to research a provider.

To add a host, put the name in the `for domain in \` loop in
`init-firewall.sh`, then restart the container (or re-run
`sudo /usr/local/bin/init-firewall.sh`):

```bash
for domain in \
    "registry.npmjs.org" \
    "api.anthropic.com" \
    "inference.poolside.ai" \
    ...
```

(Do not put a trailing `# comment` on those lines; the list uses backslash
continuations and a comment after the backslash breaks the loop.)

### Name-pinning vs CIDR ranges

The per-domain loop pins whatever IPs `dig` returned **once**, at container
start. That is fine for a host behind a small stable address set, and wrong for
one behind a large rotating pool.

Google is the worked example, already solved in `init-firewall.sh`: listing
`generativelanguage.googleapis.com` and friends by name was not enough for
Gemini CLI sign-in, because the OAuth flow redirects across hosts never resolved
at all. The symptom was `connect EHOSTUNREACH 74.125.137.95:443`, a Google
address reached via a correctly-resolved name that simply was not in the ipset.
The fix was to add Google's full published range set from
`https://www.gstatic.com/ipranges/goog.json` as CIDRs. Diagnostic rule of thumb
from that episode: an allowlisted NAME is not an allowlisted IP, and this
failure looks like DNS or a proxy problem but is neither.

Check before adding a provider, since DNS works even under the firewall:

```bash
dig +short A <host>     # a couple of stable IPs -> name-pinning is fine
                        # a CNAME into a big CDN/anycast pool -> needs CIDRs
```

Measured for the current entries: `inference.poolside.ai` is a CNAME to Baseten
(`inference.baseten.co`) behind one GCP load-balancer IP, and `openrouter.ai` is
two stable Cloudflare IPs. Both are safe to pin by name. `platform.poolside.ai`
is CloudFront with rotating IPs, but that is the browser console, used from the
Mac, so it never needs allowlisting.

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

## 5. Provider: Poolside

Gathered by web search on 2026-07-31, **not** from the vendor docs directly:
`docs.poolside.ai` is not allowlisted, so `WebFetch` could not read it from
inside the container. Treat the exact model identifier strings as unverified
until `GET /v1/models` is called after the rebuild.

- **Free tier.** Free in preview. Get a developer key at `platform.poolside.ai`
  (browser, from the Mac): sign in, API Keys tab, New key.
- **Base URL.** `https://inference.poolside.ai/v1`, OpenAI-compatible, so the
  OpenAI SDK and Aider/LiteLLM both work against it unchanged.
- **Models.** The Laguna family, which succeeded the earlier Malibu and Point
  models. Laguna M.1 is the flagship agentic-coding model (256K context, up to
  32K output); Laguna S 2.1 and Laguna XS 2.1 are the smaller ones.

Aider config, since it is an OpenAI-compatible endpoint rather than a provider
LiteLLM knows natively:

```bash
# /home/node/.aider/.env
OPENAI_API_BASE=https://inference.poolside.ai/v1
OPENAI_API_KEY=<key from platform.poolside.ai>
```

```bash
aider --model openai/<model-id-from-/v1/models> ...
```

### The OpenRouter alternative

The same Laguna models are also on OpenRouter, including `:free` variants
(`poolside/laguna-m.1:free`, `poolside/laguna-xs.2:free`), rate-limited to about
20 requests/minute and 200/day. Paid, they are among the cheapest coding models
available: Laguna XS 2.1 at $0.06/$0.12 and Laguna S 2.1 at $0.10/$0.20 per 1M
input/output tokens.

Worth considering because the plan is to try several services: allowlisting
`openrouter.ai` once opens every free model behind one host and one key, instead
of a firewall edit and a container rebuild per vendor. 200 requests/day is
ample here, since one file at `MAX_ROUNDS=2` costs roughly 5 to 6 requests.

## 6. The single-file experiment

Target: **`library/src/scala/util/Try.scala`** (batch 4, the `util` partition).
Picked over the alternatives because:

- ~50 undocumented declarations in 331 lines. Big enough to be a real workload,
  small enough to read every generated comment by hand in one sitting.
- It is core, heavily-used API, so a wrong or vague comment is obvious on sight
  rather than needing research to catch.
- Its semantics are exactly where a cheap model produces plausible-but-wrong
  prose: `Try.apply` catching only `NonFatal`, `flatMap`/`map` swallowing
  exceptions into `Failure`, and `recover` versus `recoverWith`. Good signal on
  whether the free tier is usable at all.

Rejected: the `scala.concurrent` small files (`SyncChannel`, `Channel`,
`SyncVar`) are all `@deprecated`, so they misrepresent the batch;
`DynamicVariable` is already fully documented by the earlier tag PRs;
`duration/DurationConversions.scala` is 51 near-identical unit accessors, which
tests throughput but not accuracy.

Smaller fallbacks if Try is too much for a first run: `util/Sorting.scala` (29
undocumented) or `concurrent/duration/package.scala` (24).

Run it, checking that **nothing was committed**:

```bash
cd /workspace/scala3
# 1. insert TODO FILL IN markers across the whole util partition (runs sbt)
todo-writer/scripts/run-missing-doc-todos.sh --mark-only util

# 2. keep the markers in Try.scala only, revert the rest of the partition
git diff --name-only -- library/src/scala/util \
  | grep -v '^library/src/scala/util/Try\.scala$' \
  | xargs -r git checkout --

# 3. fill just that file
aider --model openai/<model-id> \
      --message-file todo-writer/scripts/prompts/doc-writer-prompt.txt \
      --read todo-writer/docs/house-rules.md \
      --yes-always --no-auto-commits --no-gitignore \
      --map-tokens 0 --no-stream --no-check-update \
      library/src/scala/util/Try.scala

git log --oneline -1   # must be unchanged
git diff --stat        # changes must be here, uncommitted
git diff               # read every comment
```

Note the writer prompt contains a `{FILE_PATH}` placeholder that
`fill-doc-todos.sh` substitutes via `render`. For a hand-run, substitute it
first or pass the path in `--message` instead.

Throw the experiment away with `git checkout -- library/src/scala/util/`.

## 7. Rebuild checklist

1. Add the provider host to `init-firewall.sh` on `dc-template`, commit, merge
   into `dc-scala`. (`inference.poolside.ai` is already in as of 94ee1d4.)
2. Rebuild the container.
3. `aider --version` to confirm the install worked.
4. `curl -s -o /dev/null -w "%{http_code}" https://inference.poolside.ai/v1/models`
   to confirm the firewall change took.
5. Put the key in `/home/node/.aider/.env` (persisted volume).
6. `curl -s https://inference.poolside.ai/v1/models -H "Authorization: Bearer $KEY" | jq`
   to get the real model identifiers.
7. Run the experiment above.
