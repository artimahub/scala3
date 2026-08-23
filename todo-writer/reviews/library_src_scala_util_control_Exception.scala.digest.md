# Doc review digest: library/src/scala/util/control/Exception.scala

- models: writer devstral-2512 | accuracy mistral-medium-2508 | style mistral-large-2512 | adjudicator devstral-2512
- converged: false (up to 2 rounds)
- final refinement after review limit: true (not re-reviewed)
- accuracy verdict: approve
- style verdict: revise
- ADJUDICATOR verdict (final): revise

## Reviewer disagreements the adjudicator settled

- L177 `mkCatcher` -> ruled for **style**
  - accuracy: 
  - style: The documentation claims the partial function 'applies `f` to exceptions of type `Ex` that satisfy `isDef`', but the implementation throws exceptions that do not satisfy `isDef` (via `downcast(x).get`). This is misleading because the function will throw a `NoSuchElementException` if `isDef` returns false, rather than being a no-op.
  - why: The style reviewer's concern about the misleading documentation is valid and aligns with the code's behavior.
- L193 `mkThrowableCatcher` -> ruled for **style**
  - accuracy: 
  - style: The documentation does not mention that the partial function throws exceptions that do not satisfy `isDef`, similar to `mkCatcher`. This is a material omission because callers must know the function can throw.
  - why: The style reviewer's concern about the missing documentation of the throwing behavior is valid.
- L201 `throwableSubtypeToCatcher` -> ruled for **style**
  - accuracy: 
  - style: The documentation claims the catcher 'applies `pf` to exceptions of type `Ex`', but the implementation will throw a `NoSuchElementException` if `pf.isDefinedAt` returns false. This is not stated in the docs.
  - why: The style reviewer's concern about the missing documentation of the throwing behavior is valid.
- L228 `Described.withDesc` -> ruled for **style**
  - accuracy: 
  - style: The `@return` tag is redundant because the description already states 'Returns this object with the updated description'. The `@return` tag should be dropped per Scala Scaladoc conventions.
  - why: The style reviewer's suggestion to remove the redundant `@return` tag aligns with Scala Scaladoc conventions.
- L250 `Finally.and` -> ruled for **style**
  - accuracy: 
  - style: The `@return` tag is redundant because the description already states 'Returns a new Finally that executes both bodies in sequence'. The `@return` tag should be dropped per Scala Scaladoc conventions.
  - why: The style reviewer's suggestion to remove the redundant `@return` tag aligns with Scala Scaladoc conventions.
- L289 `Catch.or` -> ruled for **style**
  - accuracy: 
  - style: The first sentence of the description is a fragment ('Creates a new Catch that combines this catch's logic with another Catch's logic'). It should stand alone as a complete API-index summary, e.g., 'Combines this catch's logic with another Catch's logic.'
  - why: The style reviewer's suggestion to rewrite the first sentence for clarity is valid.
- L373 `Catch.toEither` -> ruled for **style**
  - accuracy: 
  - style: The documentation claims the method 'maps caught exceptions to Left(Throwable)', but the implementation uses `withApply(Left(_))`, which means it will wrap *any* caught exception (including those not handled by the catcher) in a `Left`. This is misleading because the method does not selectively map only caught exceptions.
  - why: The style reviewer's concern about the misleading documentation is valid and aligns with the code's behavior.
- L375 `Catch.toTry` -> ruled for **style**
  - accuracy: 
  - style: The documentation claims the method 'maps caught exceptions to Failure(Throwable)', but like `toEither`, it wraps *all* caught exceptions in a `Failure`. This is misleading for the same reason as `toEither`.
  - why: The style reviewer's concern about the misleading documentation is valid and aligns with the code's behavior.
- L383 `nonFatalCatcher` -> ruled for **style**
  - accuracy: 
  - style: The description 'a catcher that matches non-fatal throwables' is technically correct but could be more precise. The catcher *matches* non-fatal throwables (via the predicate) but *throws* them (via `throw _`). This might confuse readers about the catcher's behavior.
  - why: The style reviewer's suggestion to clarify the behavior is valid.
- L388 `allCatcher` -> ruled for **style**
  - accuracy: 
  - style: The description 'a catcher that matches all throwables' is misleading because the catcher *throws* all throwables (via `throw _`). It does not 'match' them in the sense of handling them gracefully.
  - why: The style reviewer's suggestion to clarify the behavior is valid.
- L492 `By.by` -> ruled for **style**
  - accuracy: 
  - style: The description 'Applies the handler function to create a Catch' is vague. It should clarify what the handler function does and what the resulting Catch does.
  - why: The style reviewer's suggestion to rewrite the description for clarity is valid.

## Outstanding worklist at the end

- L177 `mkCatcher` [blocker/style]: Clarify that the partial function throws a `NoSuchElementException` for exceptions that do not satisfy `isDef`.
- L193 `mkThrowableCatcher` [blocker/style]: Add a note clarifying that the partial function throws exceptions that do not satisfy `isDef`.
- L201 `throwableSubtypeToCatcher` [blocker/style]: Clarify that the catcher throws a `NoSuchElementException` for exceptions not handled by `pf`.
- L373 `Catch.toEither` [blocker/style]: Clarify that the method wraps all caught exceptions in a `Left`.
- L375 `Catch.toTry` [blocker/style]: Clarify that the method wraps all caught exceptions in a `Failure`.
- L228 `Described.withDesc` [nit/style]: Remove the `@return` tag and keep the description as-is.
- L250 `Finally.and` [nit/style]: Remove the `@return` tag and keep the description as-is.
- L289 `Catch.or` [nit/style]: Rewrite the first sentence to be declarative and self-contained: 'Combines this catch's logic with another Catch's logic.'
- L383 `nonFatalCatcher` [nit/style]: Clarify that the catcher matches non-fatal throwables and rethrows them: 'A catcher that matches non-fatal throwables and rethrows them.'
- L388 `allCatcher` [nit/style]: Clarify that the catcher matches all throwables and rethrows them: 'A catcher that matches all throwables and rethrows them.'
- L492 `By.by` [nit/style]: Rewrite the description to be more specific: 'Applies the handler function to the given argument and returns the resulting Catch object.'

## Inline NEEDS-HUMAN markers left in source
(none)
