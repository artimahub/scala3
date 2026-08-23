# Doc review digest: library/src/scala/util/Either.scala

- models: writer devstral-2512 | accuracy mistral-medium-2508 | style mistral-large-2512 | adjudicator devstral-2512
- converged: true (up to 2 rounds)
- final refinement after review limit: false (not re-reviewed)
- accuracy verdict: approve
- style verdict: approve
- ADJUDICATOR verdict (final): approve

## Reviewer disagreements the adjudicator settled


## Outstanding worklist at the end

- L523 `toTry` [nit/style]: Shorten the first sentence to: 'Converts this `Either` into a `Try`.'
- L523 `toTry` [nit/style]: Update the `@param` description to: 'evidence that the left type `A` is a subtype of `Throwable`, enabling conversion to `Failure`.'
- L565 `isLeft (Left)` [nit/style]: Simplify to: 'Returns `true`.'
- L567 `isRight (Left)` [nit/style]: Simplify to: 'Returns `false`.'
- L590 `isLeft (Right)` [nit/style]: Simplify to: 'Returns `false`.'
- L592 `isRight (Right)` [nit/style]: Simplify to: 'Returns `true`.'
- L647 `merge` [nit/style]: Simplify to: 'Extracts the contained value from this `Either`.'

## Inline NEEDS-HUMAN markers left in source
(none)
