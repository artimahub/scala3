# Doc review digest: library/src/scala/quoted/ExprMap.scala

- models: writer opus | accuracy gpt-5.6-terra | style sonnet | adjudicator sonnet
- converged: false (up to 3 rounds)
- final refinement after review limit: true
- verification review of that refine: revise
- a real accuracy review ran at some point: true
- the file AS IT NOW STANDS was accuracy-reviewed: true
- accuracy verdict: revise
- style verdict: approve
- ADJUDICATOR verdict (final): revise

> **NOT REVIEWED.** Do not put this file in a PR on the strength of this
> digest. Read the diff yourself, or re-run the file once the reviewer is
> healthy. See reviews/NOT-REVIEWED.txt.

## Reviewer disagreements the adjudicator settled


## Outstanding worklist at the end

- L267 `transformTypeTrees` [blocker/accuracy]: State that `owner` is passed to `transformTypeTree`, which ignores it, so its value has no effect.
- L285 `transformTypeCaseDefs` [blocker/accuracy]: State that `owner` is passed through `transformTypeCaseDef` to `transformTypeTree`, which ignores it, so its value has no effect.
- L6 `ExprMap` [nit/accuracy]: Reword the trait summary as a declarative, verb-first sentence, e.g. "Maps quoted expressions to expressions of the same type."
- L18 `transformChildren` [nit/style]: Reword the `@return` from present-perfect ("have been replaced") to simple present ("are replaced") for tense consistency with the rest of the file.
- L42 `transformStatement` [nit/style]: Optionally reword the repeated `@param owner the symbol that owns \`tree\`` tag (applies to all 4 occurrences, lines 42, 90, 167, 200) to something like "the current owner symbol in whose scope `tree` is transformed" for precision.

## Inline NEEDS-HUMAN markers left in source
(none)
