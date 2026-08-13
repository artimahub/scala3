# Free-model writer study

Can a free-tier model fill missing Scaladoc well enough to drive the
missing-documentation PR schedule (`docs/missing-doc-schedule.md`), so the
remaining ~8 PRs stop consuming Claude Code and Codex budget?

**Yes.** Twenty-six trials across four providers and three clients.

## The answer, up front

| Role | Model | Provider | Why |
|---|---|---|---|
| Writer + adjudicator | **`zai-glm-5-2`** | Mistral | 23s, full fill, 1 redundant `@return`, both accuracy probes right |
| Accuracy reviewer | **`gemma-4-31b`** | Cerebras | passes both accuracy probes; different family from the writer |
| Style reviewer | **`mistral-medium-latest`** or `gpt-oss-120b` | Mistral / Cerebras | 2 and 1 redundant tags -- the best convention-followers |

All free. Three independent model families, which is what makes reviewer
disagreement meaningful.

The bigger finding is not about models at all: **most apparent model failures in
this study were client failures.** See "The harness mattered more than the
models".

## The test

Every trial fills the **same input**: `library/src/scala/util/Try.scala` with
todo-writer's markers -- 42 declarations, 128 `TODO FILL IN` placeholders.
Committed on `exp/try-baseline`, copied to `try-scala/Try.scala.MARKED-input`.

`run-writer-trial.sh <label> <provider> <model>` resets the file, loops until the
markers are gone, and scores:

1. **Code integrity** -- strip comment lines before and after, diff them. Any
   change to a non-comment line fails the round, which is rolled back.
2. **Markers left** -- 0 means fully filled.
3. **Redundant `@return`** -- the project's DROP rule: delete `@return` when the
   description already begins "Returns" and states the whole return value.
4. **Trailing periods on tag lines** -- a house-style deviation.

Plus two hand-checked accuracy probes, chosen because they are where a model
writes plausible-but-wrong prose:

- `Failure.isSuccess` returns `false`. Does the doc say so, or does it just
  describe the method's *name*?
- `Failure.orElse` is `try default catch { case NonFatal(e) => Failure(e) }`.
  Is that guarantee documented at all?

## Results

Work time excludes the runner's own between-round pauses. Provider rate-limit
backoff is left in, being a real cost of that service.

### Models that completed the file

| Model | Provider | Client | Time | Redundant `@return` | `isSuccess` | `NonFatal` |
|---|---|---|---:|---:|---|---|
| **`zai-glm-5-2`** | Mistral | direct | **23s** | **1** | yes | yes |
| `qwen3-coder-next` | OpenRouter (paid) | direct | 26s | **1** | yes | yes |
| `gpt-oss-120b` | Cerebras | direct | 11s | **1** | **no** | yes |
| `mistral-medium-latest` | Mistral | direct | 72s | 2 | yes | yes |
| `gemma-4-31b` | Cerebras | direct | 3s | 10 | yes | yes |
| `devstral-medium-latest` | Mistral | direct | 56s | 10 | yes | yes |
| `codestral-latest` | Mistral | direct | 71s | 11 | yes | yes |
| `mistral-code-latest` | Mistral | direct | 60s | 11 | **no** | yes |
| `qwen3-coder` | OpenRouter (paid) | direct | 126s | 11 | yes | yes |
| `qwen3-coder-plus` | OpenRouter (paid) | direct | 61s | 11 | yes | yes |
| `zai-glm-4.7` | Cerebras | direct | 5s | 12 | yes | yes |
| `qwen3-coder-30b-a3b` | OpenRouter (paid) | direct | 143s | 14 | yes | yes |
| `mistral-large-latest` | Mistral | direct | 194s | 15 | yes | yes |
| `gpt-oss-120b` | Cerebras | aider | 319s | 1 | no | yes |
| `laguna-s-2.1` | poolside | pool | ~2040s | 1 | yes | yes |

`laguna-s-2.1`'s file was lost to a rebuild before `experiments/` existed; its
metrics come from the session record.

### Models that did not

| Model | Provider | Time | Markers | Why |
|---|---|---:|---:|---|
| `devstral-latest` | Mistral | 157s | 14 left | stalled; 4 blocks never matched |
| `laguna-s-2.1:free` | OpenRouter | 181s | 64 left | slow, half done in 2 rounds |
| `laguna-xs-2.1:free` | OpenRouter | 280s | 122 left | **deleted a method**, rolled back |
| `glm-4.7-flash` | OpenRouter (paid) | 201s | 122 left | 56 blocks parsed, 3 applied |
| `north-mini-code:free` | OpenRouter | 1011s | 128 left | reasoning=32,910, empty content |
| `nemotron-3.5-lightning:free` | OpenRouter | 100s | 128 left | 25,173/26,550 tokens on reasoning |
| `nemotron-3-super-120b:free` | OpenRouter | 601s | 128 left | timed out, no response |
| `gpt-oss-20b:free` | OpenRouter | 0s | 128 left | HTTP 429, shared upstream pool |
| `gemma-4-26b-a4b-it:free` | OpenRouter | 348s | 128 left | 0 blocks -- wrong format |
| `ling-3.0-tiny:free` | OpenRouter | 13s | 128 left | 0 blocks -- wrong format |
| `gpt-oss-120b` (paid) | OpenRouter | 420s | 128 left | timed out; **works fine on Cerebras** |
| `zai-glm-4.7` | Cerebras (aider) | 60s | 128 left | empty content -- aider's fault, see below |

A `redundant @return` score on a failed run measures the unfilled baseline, not
the model. Ignore it for anything in this table.

## What we learned about models

**Two capabilities, and they used to come apart.** Following the conditional
`@return` DROP rule and writing accurate prose looked like separate skills:
`gpt-oss-120b` scored 1 on the rule but wrote a wrong `isSuccess`, while
`gemma-4-31b` and `zai-glm-4.7` were accurate but scored 10 and 12. Running all
three through the identical path with the identical prompt confirmed it was a
model property, not prompt wording -- raising the DROP example into the prompt's
format section moved GLM only from 14 to 12.

**Newer models do both.** `zai-glm-5-2` scores **1** where its predecessor
`zai-glm-4.7` scored 12, and passes both accuracy probes. Same family, one
version apart. So the split was a model-maturity artifact, not a law.

**Code specialisation did not predict quality.** A reasonable prior, and wrong:
`codestral` (11), `mistral-code` (11) and `devstral-medium` (10) all trailed the
general-purpose `mistral-medium` (2) and `zai-glm-5-2` (1). `mistral-code-latest`
was also one of only two models to write a wrong `isSuccess`.

**Reasoning models are a hazard here.** They emit into a separate `reasoning`
channel before any `content`, and with a large output budget they can spend all
of it thinking. `north-mini-code` burned 32,910 reasoning tokens and returned
nothing, in 16 minutes. Disabling reasoning is provider-specific:
`reasoning_effort: "none"` on Cerebras, `reasoning: {"enabled": false}` on
OpenRouter. Send the wrong spelling and it is silently ignored.

**Block style is a risk signal.** Models emitting few, very large blocks
(qwen filled the file with 3 blocks of 30, 116 and 114 lines) must reproduce
100+ lines byte-exactly. That is where both code deletions happened. A model
emitting 42 small blocks has far more margin.

## The harness mattered more than the models

Most of the elapsed effort went here, because failures kept looking like model
limitations when they were client problems.

| Client | Outcome |
|---|---|
| **aider** | broke 3 of 4 models, each differently |
| **Codex CLI** | its own tool router rejected the model's call: `unsupported call: read` |
| **pool** | worked first time for laguna; cannot reach Cerebras (sends `cache_control` fields it rejects) |
| **direct** | worked for every model tried |

Aider's three failures:

- `laguna-s-2.1` could not emit SEARCH/REPLACE at all. Whole-file mode complied
  but **silently deleted the `Failure` case class**, 35 lines, while the marker
  count, `edits applied: 1` and a plausible diffstat all reported success.
- `gpt-oss-120b` was fed lint errors from a tree-sitter grammar that cannot parse
  Scala 3 capture checking, so it was told to "fix" correct code like
  `def flatMap[U](f: T => Try[U]^): Try[U]^{this, f}`. That wasted rounds and
  actively invited the model to edit Scala, which the prompt forbids.
- `zai-glm-4.7` returned empty `content` in three configurations. Through
  `direct`, the same model, file and endpoint filled everything in 5 seconds.

`direct-writer.py` exists because of that: one user message, no system prompt, no
few-shot examples, no linter. It parses SEARCH/REPLACE blocks and applies each
only where its SEARCH text occurs **exactly once**, so the file can only change
where a block matched.

Three bugs in it, all found only by running it live:

- **HTTP 403 `error code: 1010`** -- Cerebras and OpenRouter sit behind
  Cloudflare, which rejects urllib's default `Python-urllib/3.11` User-Agent as a
  bot signature. Every earlier probe used curl, so it never showed up.
- **Zero blocks parsed** -- the aider prompt only *names* the SEARCH/REPLACE
  format; aider supplies the literal shape via its system prompt and few-shot
  examples. Sent bare, models replied with plain ```scala fences.
  `doc-writer-prompt-direct.txt` spells the format out literally.
- **JSONDecodeError** -- OpenRouter emits keepalive lines (blanks and spaces)
  *before* the JSON body during long generations. `json.loads` rejects that; `jq`
  tolerates it, which is why every curl probe worked and the first real run died.

## The integrity guard earned its place twice

Two models silently removed code while filling comments, and **only the
strip-comments diff noticed**:

- `laguna-s-2.1` under aider's whole-file mode deleted the entire `Failure` case
  class (35 lines), leaving 68 dangling references. The file could not compile.
- `laguna-xs-2.1:free` filled 107 markers and deleted
  `override def foreach[U](f: T => U): Unit = ()` in the same round.

In both cases every other signal read as success: `finish_reason: stop`, blocks
parsed and applied, a fast-dropping marker count, a plausible diffstat. Keep this
check in the real pipeline regardless of which model wins.

## Providers

| Provider | Cost | Verdict |
|---|---|---|
| **Mistral** | free ("Experiment", ~1B tokens/month) | **best free tier.** All models, generous quota, every trial passed integrity |
| **Cerebras** | free | fast (3-11s), dedicated capacity, but only 3 models and `zai-glm-4.7` is deprecated 17 Aug |
| **OpenRouter** `:free` | free | **unusable.** Shared upstream pools: 429s, timeouts, 8 models tried, 0 usable |
| **OpenRouter** paid | ~$0.11-0.32 per PR | works well; useful for models nobody else hosts |
| **poolside** | free | works via `pool`, but ~34 min per file |

**On OpenRouter paid**: a whole 36-file PR costs 11-32 cents, so the $10 credit
covers the remaining schedule ~30 times over. Optimising for "free" was largely
the wrong goal -- **reviewer budget dominates writer cost.** One extra review
round across 36 files costs more Claude Code budget than the entire OpenRouter
bill for the campaign.

**Same model, different service, different result**: `gpt-oss-120b` fills the
file in 11s on Cerebras and times out at 420s through OpenRouter. Which service
routes a model matters as much as the model.

## Branch map

| Branch | Holds |
|---|---|
| `exp/try-baseline` | `Try.scala` with its 128 markers -- the fixed starting point |
| `exp/try-mistral` | all 7 Mistral trials |
| `exp/try-paid` | all 6 paid-OpenRouter trials |
| `exp/try-openrouter` | all 8 free-OpenRouter trials |
| `exp/try-gpt-oss-120b` / `-direct` | aider and direct runs |
| `exp/try-gemma-4-31b` | direct run |
| `exp/try-zai-glm-4.7` | three aider failures **and** the working direct run |
| `exp/try-north-mini-code` | first OpenRouter attempt, pre-fixes |
| `exp/wk4-marked-wip` | the full week-4 partition marked, 36 files |
| `feature-todo-writer` | the harness: runner, `direct-writer.py`, prompts, pipeline |

Each trial branch carries the filled `Try.scala`, a `.result.txt` scorecard, the
raw model reply and the logs. Logs survive because `experiments/.gitignore`
negates upstream's `*.log` rule -- without it, the diagnostics every finding here
rests on would be silently dropped.

## Gotchas

- **A client's exit code is not the completion signal.** aider exited 0 having
  abandoned its retries with 63 of 128 markers left. The marker count decides.
- **`grep -c` exits 1 when the count is zero**, so `grep -c ... || echo 0` prints
  "0\n0" and breaks every numeric test downstream. This bit the loop script and
  a status check.
- **Model ids from search results and blog posts go stale.**
  `qwen/qwen3-coder-480b:free` does not exist; every qwen3-coder that does is
  paid. Always check `/v1/models` first.
- **A `:free` id is a different model from its paid twin.** Dropping the suffix
  silently switches to the paid one. The runner now refuses any OpenRouter model
  whose live `pricing.prompt` is not `0` unless `ALLOW_PAID=1`, and prices the
  run before spending.
- **A small viability probe does not predict real behaviour.** A 400-token "say
  PONG" said three OpenRouter models were healthy and fast; all three then failed
  on an 8K-in/32K-out request.
- **Commit before switching branches.** Uncommitted results ride along silently
  and can land on the wrong branch.
- **`/tmp` and `/home/node` are overlay storage** and are destroyed by a
  devcontainer rebuild. An early set of generated files was lost that way. Only
  `/workspace` (bind mount) and the named volumes survive.
- **Aider drops `.aider*` files in the git root**, and `.aider*` is not gitignored
  in scala3 -- a public fork PRs are opened from.

## Still open

- **Nothing has been run through the full pipeline.** Every result here is
  writer-only; the reviewers and adjudicator in `fill-doc-todos-poolside.sh` are
  untested, and they are what would catch the defects the guards merely count.
- Re-run the winner on a **larger file** (`Future.scala` at 952 lines,
  `Duration.scala` at 785). One 519-line file is one data point, and the
  large-block style is exactly where code deletions happened.
- Re-run `laguna-s-2.1` through `pool` to replace the output lost to the rebuild.
- Local quantized models are reachable from the container without a firewall
  change (`host.docker.internal`, host network is allowed). Worth testing
  `qwen3-coder-next` at Q4_K_M against the bf16/fp8 OpenRouter result -- the
  large-block style makes it unusually sensitive to quantization.
