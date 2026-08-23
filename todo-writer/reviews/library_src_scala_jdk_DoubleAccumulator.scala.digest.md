# Doc review digest: library/src/scala/jdk/DoubleAccumulator.scala

- models: writer opus | accuracy gpt-5.6-terra | style sonnet | adjudicator sonnet
- converged: true (up to 3 rounds)
- final refinement after review limit: false
- verification review of that refine: skipped
- a real accuracy review ran at some point: true
- the file AS IT NOW STANDS was accuracy-reviewed: true
- accuracy verdict: approve
- style verdict: approve
- ADJUDICATOR verdict (final): approve

## Reviewer disagreements the adjudicator settled


## Outstanding worklist at the end

- L565 `DoubleAccumulatorStepper.nextStep` [nit/both/NEEDS-HUMAN]: Keep the existing @note NEEDS-HUMAN on nextStep as-is pending maintainer confirmation of whether the `n <= 0` guard (versus the siblings' `N <= 0`) is intentional; no other wording change needed.
- L178 `DoubleAccumulator.update` [nit/style]: In both occurrences of the update(idx, elem) out-of-range prose (lines 178 and 201), add that a wrapped offset can also land on a history block's trailing bookkeeping slot (the cumulative-length value read by `cumulative`/`seekSlot`), corrupting the accumulator's internal indexing rather than overwriting a reported element.

## Inline NEEDS-HUMAN markers left in source
- L561:   *  @note NEEDS-HUMAN: the guard here is `n <= 0` (the size of the current block), whereas
