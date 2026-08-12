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
| `cohere/north-mini-code:free` | direct / OpenRouter | \_ | \_ | \_ | \_ | \_ | \_ | \_ | `exp/try-qwen3-coder` |

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
| `exp/try-qwen3-coder` | OpenRouter run (branch name predates the model choice) |
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
- **Commit before switching branches.** Uncommitted results ride along silently
  and can land on the wrong branch.

## Still open

- Re-run `laguna-s-2.1` through `pool` to replace the lost output.
- Finish the OpenRouter trials. Free models with adequate output budget:
  `cohere/north-mini-code:free` (the only code-specialised one),
  `nvidia/nemotron-3-super-120b-a12b:free`, `nvidia/nemotron-3-ultra-550b-a55b:free`,
  `openai/gpt-oss-20b:free`. `poolside/laguna-s-2.1:free` and
  `google/gemma-4-31b-it:free` are useful **controls** -- same models measured
  elsewhere, so they isolate what OpenRouter's routing itself changes.
- Test a writer through the full pipeline (`fill-doc-todos-poolside.sh`), since
  every result here is writer-only. The reviewers and the adjudicator are what
  would catch the defects the guards above only count.
