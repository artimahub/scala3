# Free-tier writer trials: results

Which free-tier model can fill missing Scaladoc well enough to drive the
missing-documentation PR schedule (`docs/missing-doc-schedule.md`), so the
remaining ~8 PRs do not consume Claude Code or Codex budget.

## The test

Every trial fills the **same input**: `library/src/scala/util/Try.scala` with
todo-writer's markers in place -- 42 declarations, 128 `TODO FILL IN`
placeholders. Committed on `exp/try-baseline` and copied to
`experiments/try-scala/Try.scala.MARKED-input`.

Driven by `run-writer-trial.sh <label> <provider> <model>`, which resets the
file, loops until the markers are gone, and scores four guards:

1. **Code integrity** -- strip comment lines before and after, diff them. A run
   once deleted an entire public class while the marker count, the edit count
   and the diffstat all reported success; only this caught it.
2. **Markers left** -- 0 means fully filled.
3. **Redundant `@return`** -- the project's DROP rule, the most-missed convention.
4. **Trailing periods on tag lines** -- a house-style deviation.

Plus two accuracy spot-checks done by hand, chosen because they are where a
model produces plausible-but-wrong prose:

- `Failure.isSuccess` returns `false`. Does the doc say so, or does it describe
  the method's *name*?
- `Failure.orElse` is `try default catch { case NonFatal(e) => Failure(e) }`.
  Is that guarantee documented at all?

## Results

| Model | Provider / client | Rounds | Work time | Markers | Integrity | Redundant `@return` | `isSuccess` right | `NonFatal` documented | Branch |
|---|---|---|---|---|---|---|---|---|---|
| `gpt-oss-120b` | direct / Cerebras | 2 | **11s** | 128 -> 0 | PASS | **1** | no | yes | `exp/try-gpt-oss-120b-direct` |
| `gemma-4-31b` | direct / Cerebras | 1 | **3s** | 128 -> 0 | PASS | 10 | yes | yes | `exp/try-gemma-4-31b` |
| `zai-glm-4.7` | direct / Cerebras | 1 | **5s** | 128 -> 0 | PASS | 12 | yes | yes | `exp/try-zai-glm-4.7` |
| `gpt-oss-120b` | aider / Cerebras | 3 | 319s | 128 -> 0 | PASS | 1 | no | yes | `exp/try-gpt-oss-120b` |
| `zai-glm-4.7` | aider / Cerebras | 1 | 60s | **128 -> 128** | n/a | n/a | n/a | n/a | `exp/try-zai-glm-4.7` |
| `laguna-s-2.1` | pool / poolside | 1 | ~2040s | 128 -> 0 | PASS | 1 | yes | yes | output lost, see below |

Work time excludes the runner's own between-round pauses; rate-limit backoff
inside a round is left in, since that is a real cost of a free tier.

`laguna-s-2.1`'s generated file was lost to a devcontainer rebuild before
`experiments/` existed (it was written to `/tmp`, which is overlay storage). The
metrics are from the session record; the run itself passed every guard. It is
worth re-running for a complete table.

## What the numbers say

**No model wins outright.** The two capabilities that matter come apart:

- `gpt-oss-120b` is far the best at the conditional `@return` DROP rule -- 1
  violation against 10 and 12 -- and is the *only* model that writes a wrong
  doc: `Indicates that this Try is a Success` on `isSuccess: Boolean = false`.
- `gemma-4-31b` and `zai-glm-4.7` get the accuracy checks right and document the
  `NonFatal` guarantee more thoroughly, but neither reliably applies a
  two-condition rule.

That split is a **model property, not a prompt problem**. All three were run
through the identical code path with the identical prompt specifically to test
this, after an attempt to fix it by raising the DROP worked example into the
prompt's format section moved GLM only from 14 to 12.

The practical reading: pick a writer for accuracy and let the reviewers catch
convention violations. Redundant `@return` is exactly what the style reviewer
flags; a wrong `isSuccess` is exactly what the Codex accuracy reviewer flags.

## The harness mattered more than the models

Getting a usable answer took longer than it should have because failures kept
looking like model limitations when they were client problems.

| Client | Outcome |
|---|---|
| **aider** | Broke 3 of 4 models, each differently (below) |
| **Codex CLI** | Its own tool router rejected the model's call: `unsupported call: read` |
| **pool** | Worked first time for laguna; cannot reach Cerebras (sends `cache_control` fields it rejects) |
| **direct** | Worked for every model tried |

Aider's three failures:

- `laguna-s-2.1` could not emit SEARCH/REPLACE at all. Whole-file mode complied
  but **silently deleted the `Failure` case class**, 35 lines, while the marker
  count, `edits applied: 1` and a plausible diffstat all reported success.
- `gpt-oss-120b` was fed lint errors from a tree-sitter grammar that cannot
  parse Scala 3 capture checking, so it was asked to "fix" correct code like
  `def flatMap[U](f: T => Try[U]^): Try[U]^{this, f}`. That both wasted rounds
  and invited the model to edit Scala, which the prompt forbids.
- `zai-glm-4.7` returned empty `content` in three configurations. Through
  `direct` the same model, file and endpoint filled everything in 5 seconds.

`direct-writer.py` exists because of that: one user message, no system prompt,
no few-shot examples, no linter. It parses SEARCH/REPLACE blocks and applies
each only where its SEARCH text occurs **exactly once**, so the file can only
change where a block matched.

Two bugs found only by running it live, both now fixed:

- **HTTP 403 `error code: 1010`** -- Cerebras and OpenRouter both sit behind
  Cloudflare, which rejects urllib's default `Python-urllib/3.11` User-Agent as
  a bot signature. Every earlier probe used curl, so it never showed up.
- **Zero blocks parsed** -- the aider prompt only *names* the SEARCH/REPLACE
  format; aider supplies the literal shape itself. Sent bare, GLM replied with
  plain ```scala fences. `doc-writer-prompt-direct.txt` spells the format out.

## Branches

| Branch | Holds |
|---|---|
| `exp/try-baseline` | `Try.scala` with its 128 markers -- the fixed starting point |
| `exp/try-gpt-oss-120b` | aider run |
| `exp/try-gpt-oss-120b-direct` | direct run |
| `exp/try-zai-glm-4.7` | three aider failures **and** the working direct run |
| `exp/try-gemma-4-31b` | direct run |
| `exp/try-openrouter` | all eight OpenRouter trials |
| `exp/try-north-mini-code` | the first OpenRouter attempt, before the reasoning and keepalive fixes |
| `exp/wk4-marked-wip` | the full week-4 partition marked, 36 files |
| `feature-todo-writer` | the harness: runner, `direct-writer.py`, prompts, pipeline |

Each trial branch carries the filled `Try.scala`, a `.result.txt` scorecard, the
raw model reply (`.reply.txt`) and the logs. Logs survive because
`experiments/.gitignore` negates upstream's `*.log` rule -- without it the
diagnostics every finding here rests on would be silently dropped.

## Gotchas worth remembering

- **A client's exit code is not the completion signal.** aider exited 0 having
  abandoned its retries with 63 of 128 markers left. The marker count decides.
- **`qwen/qwen3-coder-480b:free` does not exist.** Model ids from search results
  and blog posts go stale; check `/v1/models` and confirm `pricing.prompt == 0`
  before building a trial around one.
- **OpenRouter's $10 raises the free-model daily cap from 50 to 1000 requests.**
  Irrelevant for these trials (a whole file is 1-2 requests) but necessary for a
  real partition: 36 files x ~2 rounds is ~72 writer requests.
- **A `:free` id is a different model from its paid twin.** Dropping the suffix
  silently switches to the paid one. `run-writer-trial.sh` now refuses any
  OpenRouter model whose live `pricing.prompt` is not `0`, and refuses ids that
  are not in the catalogue at all.
- **Commit before switching branches.** Uncommitted results ride along silently
  and can land on the wrong branch.

## OpenRouter free tier

Tried after Cerebras, on the same input through the same `direct` path. Branch:
`exp/try-openrouter`, which holds every OpenRouter result together.

**Nothing usable came out of it.** Eight models tried; not one both filled the
file and passed the guards.

| Model | Reasoning | Work time | Markers | Integrity | What happened |
|---|---|---|---:|---|---|
| `openai/gpt-oss-20b:free` | default | 0s | 128 -> 128 | n/a | HTTP 429, rate-limited upstream |
| `nvidia/nemotron-3-super-120b-a12b:free` | default | 601s | 128 -> 128 | n/a | timed out, no response at all |
| `nvidia/nemotron-3.5-lightning:free` | default | 100s | 128 -> 128 | PASS | 25,173 of 26,550 tokens on reasoning; 4 blocks, none matched |
| `cohere/north-mini-code:free` | default | 1011s | 128 -> 128 | PASS | reasoning=32,910, `content` empty, 0 blocks |
| `poolside/laguna-s-2.1:free` | off | 181s | 128 -> **64** | PASS | worked, but slow and only half done in 2 rounds |
| `poolside/laguna-xs-2.1:free` | off | 280s | 128 -> 122 | **FAIL** | **deleted a method**; round rolled back |
| `google/gemma-4-26b-a4b-it:free` | off | 348s | 128 -> 128 | PASS | replied, but 0 blocks -- wrong format |
| `inclusionai/ling-3.0-tiny:free` | off | 13s | 128 -> 128 | PASS | replied, but 0 blocks -- wrong format |

### The three failure modes

**Shared upstream pools.** `:free` variants route through a pool shared with
every other free user, so you queue behind them. gpt-oss-20b returned
`limit_source: upstream_provider_shared_pool` before doing any work, and
nemotron-super-120b never answered at all. This is structurally different from
Cerebras, where an own key against their own hardware finished the same file in
3-12 seconds.

**Reasoning eats the budget.** Free models here skew towards reasoning models,
which emit into a separate channel before any `content`. Given a 32K budget they
can spend all of it thinking: north-mini-code burned 32,910 reasoning tokens and
returned nothing, in 16 minutes. Disabling reasoning is what made laguna work at
all, and OpenRouter spells that `reasoning: {"enabled": false}` where Cerebras
uses `reasoning_effort: "none"` -- send the wrong one and it is silently ignored.

**Smaller models cannot produce the format.** gemma-4-26b and ling-3.0-tiny both
replied normally with reasoning off -- `finish_reason: stop`, thousands of
content tokens -- and yielded **zero** parseable blocks. They wrote something
else entirely. On Cerebras the same size class (gemma-4-31b) managed 42 of 42,
so this is about these particular models, not about model size as such.

**A viability probe does not predict real behaviour.** A 400-token "say PONG"
request said north-mini-code, nemotron-super-120b and gpt-oss-20b were all
healthy and fast. All three then failed on an 8K-token prompt asking for 32K of
output. The shared pool copes with toy requests and collapses under real ones.

### The guard caught a live code deletion

`poolside/laguna-xs-2.1:free` round 2 filled 107 markers (122 -> 15) and, while
doing it, deleted a public method:

```
54d53
<   override def foreach[U](f: T => U): Unit = ()
```

The strip-comments integrity check caught it, rolled the whole round back, and
stopped the trial. Every other signal looked like success: `finish_reason: stop`,
21 blocks parsed, 18 applied, a big drop in the marker count. Without the guard
that is a PR with a missing method and 107 plausible new comments.

This is the second time a model has silently removed code while filling comments
-- laguna-s-2.1 deleted the entire `Failure` class under aider's whole-file mode.
Both times it was the same model family, and both times only this check noticed.

### One thing OpenRouter did establish

`laguna-s-2.1` emitted valid SEARCH/REPLACE blocks here (9 parsed, 9 applied),
which it could never do through aider. So its earlier failure was aider's
prompt scaffolding, not an inability to produce the format -- consistent with
what the GLM investigation found.

### Verdict

Cerebras is the better free tier for this workload by a wide margin: dedicated
capacity, 3-12 second fills, and three models that complete the file. OpenRouter's
value is breadth of catalogue, not throughput, and the shared free pool is not
suited to an 8K-in/32K-out job repeated across 36 files.

The $10 credit is not wasted -- it raises the free daily cap from 50 to 1000
requests, and OpenRouter remains the only route to models no one else hosts. But
it is not the path for the PR schedule.

## Paid OpenRouter models -- and the answer

The free-tier failures prompted the obvious question: how much do the paid
models actually cost? Measured from 8 real runs (avg 8,520 prompt + 11,701
completion tokens per request), a **whole 36-file PR is 11 to 32 cents**. The
$10 credit covers the entire remaining schedule roughly 30 times over.

So "free tier" was the wrong thing to optimise. Six paid models on the same
`Try.scala`, under 25 cents for all six combined. Branch: `exp/try-paid`.

| Model | Work time | Markers | Integrity | Redundant `@return` | `isSuccess` right | `NonFatal` | PR cost |
|---|---:|---:|---|---:|---|---|---:|
| **`qwen/qwen3-coder-next`** | **26s** | 128 -> 0 | PASS | **1** | **yes** | **yes** | ~$0.37 |
| `qwen/qwen3-coder-plus` | 61s | 128 -> 0 | PASS | 11 | yes | yes | ~$1.57 |
| `qwen/qwen3-coder` | 126s | 128 -> 0 | PASS | 11 | yes | yes | ~$0.51 |
| `qwen/qwen3-coder-30b-a3b-instruct` | 143s | 128 -> 0 | PASS | 14 | yes | yes | ~$0.24 |
| `z-ai/glm-4.7-flash` | 201s | 128 -> 122 | PASS | 0\* | - | - | ~$0.32 |
| `openai/gpt-oss-120b` | 420s | 128 -> 128 | n/a | - | - | - | ~$0.14 |

\* glm-4.7-flash filled almost nothing (56 blocks parsed, 3 applied, 39 did not
match), so its zero is an artifact of an empty run, not good behaviour.

### qwen3-coder-next is the answer

It is the **first model to do both things well**. The whole comparison until now
was a trade-off: gpt-oss-120b followed the conditional `@return` DROP rule (1
violation) but wrote a wrong `isSuccess`; GLM and gemma were accurate but left
10-12 redundant tags. qwen3-coder-next has 1 redundant tag AND both accuracy
probes right, in 26 seconds, for about 37 cents per PR.

That it is a **coding-specialised** model is the likeliest explanation, and it
was the class of model this search wanted from the start -- `qwen3-coder-480b`
was the original target before it turned out not to exist.

### gpt-oss-120b: the same model, different service

It failed twice through OpenRouter -- first `Reasoning is mandatory for this
endpoint and cannot be disabled`, then a 420s timeout with reasoning left on --
while being fast and reliable on Cerebras (11s, full fill). Same weights, same
prompt, same harness. Which service routes a model matters as much as the model.

### A caveat on the qwen block style

The qwen models emit **very few, very large** blocks: qwen3-coder-30b filled the
file with 3 blocks of 30, 116 and 114 lines. That means reproducing 100+ lines
verbatim, which is exactly where laguna deleted a public method and a public
class. Integrity passed every time here, but the margin is thinner than a model
emitting 42 small blocks. Keep the integrity guard on for these especially.

## Still open

- Re-run `laguna-s-2.1` through `pool` to replace the lost output.
- Re-run `qwen3-coder-next` on a second file before committing to it; one file
  is one data point, and its few-large-blocks style deserves more evidence.
- Finish the OpenRouter trials. Free models with adequate output budget:
  `cohere/north-mini-code:free` (the only code-specialised one),
  `nvidia/nemotron-3-super-120b-a12b:free`, `nvidia/nemotron-3-ultra-550b-a55b:free`,
  `openai/gpt-oss-20b:free`. `poolside/laguna-s-2.1:free` and
  `google/gemma-4-31b-it:free` are useful **controls** -- same models measured
  elsewhere, so they isolate what OpenRouter's routing itself changes.
- Test a writer through the full pipeline (`fill-doc-todos-poolside.sh`), since
  every result here is writer-only. The reviewers and the adjudicator are what
  would catch the defects the guards above only count.
