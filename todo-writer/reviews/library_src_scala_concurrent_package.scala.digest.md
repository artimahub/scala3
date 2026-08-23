# Doc review digest: library/src/scala/concurrent/package.scala

- models: writer devstral-2512 | accuracy mistral-medium-2508 | style mistral-large-2512 | adjudicator devstral-2512
- converged: false (up to 2 rounds)
- final refinement after review limit: true (not re-reviewed)
- accuracy verdict: approve
- style verdict: revise
- ADJUDICATOR verdict (final): revise

## Reviewer disagreements the adjudicator settled

- L220 `FutureValue.unapply` -> ruled for **style**
  - accuracy: 
  - style: The `@return` tag claims the method returns `Some(Try[T])` if `a` is a `Future`, but the implementation returns the raw `Try[T]` from `f.value` directly, not wrapped in `Some`. The actual return type is `Option[Try[T]]`, where `Future` cases return `Some(Try[T])` and non-`Future` cases return `None`. The description conflates the `Option` wrapper with the `Try` value.
  - why: The accuracy reviewer did not raise any issues, while the style reviewer correctly identified an inaccuracy in the return type description.

## Outstanding worklist at the end

- L220 `FutureValue.unapply` [blocker/style]: Revise the `@return` tag to: `@return `Some(Try[T])` if `a` is a `Future`, otherwise `None` (i.e., the raw `Option[Try[T]]` from pattern matching).`
- L220 `FutureValue.unapply` [nit/style]: Revise the first sentence to: `Extracts the value of a `Future` as `Some(Try[T])` if the `Awaitable` is a `Future`, otherwise `None`.
- L222 `FutureValue.unapply` [nit/style]: Revise the `@param` tag to: `@param a the `Awaitable` whose value is extracted if it is a `Future`.

## Inline NEEDS-HUMAN markers left in source
(none)
