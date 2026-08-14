# Doc review digest: library/src/scala/util/Try.scala

- models: writer zai-glm-5-2 | accuracy mistral-medium-latest | style mistral-large-latest | adjudicator zai-glm-5-2
- converged: true (up to 2 rounds)
- final refinement after review limit: false (not re-reviewed)
- accuracy verdict: revise
- style verdict: revise
- ADJUDICATOR verdict (final): approve

## Reviewer disagreements the adjudicator settled

- L165 `Try.WithFilter#map` -> ruled for **neither**
  - accuracy: Blocker: @return omits the NoSuchElementException case when p fails.
  - style: Blocker: same omission of the Failure case.
  - why: The current @return already reads 'or a `Failure` with a `NoSuchElementException` if it does not', so the cited omission is not present in the diff.
- L170 `Try.WithFilter#flatMap` -> ruled for **neither**
  - accuracy: Blocker: omits the NoSuchElementException case when p fails.
  - style: 
  - why: The current @return already states 'or a `Failure` with a `NoSuchElementException` if it does not'.
- L297 `Failure#orElse` -> ruled for **neither**
  - accuracy: Blocker: phrasing implies default is wrapped, should say 'in the resulting Try'.
  - style: Blocker: omits the non-fatal exception guarantee.
  - why: The current @return already states 'any non-fatal exception thrown while evaluating `default` is caught and returned as a `Failure`'; accuracy's wrapping concern is unfounded since default is already a Try[U].
- L313 `Failure#transform` -> ruled for **neither**
  - accuracy: Blocker: ambiguous, rephrase to 'the Try produced by applying f'.
  - style: Blocker: omits the non-fatal exception guarantee.
  - why: Current @return already states 'the `Try` returned by `f` applied to the exception; any non-fatal exception thrown by `f` is caught and returned as a `Failure`'; both cited issues are absent from the diff.
- L325 `Failure#recover` -> ruled for **neither**
  - accuracy: Blocker: should clarify v is pf(exception) result and exception case.
  - style: Blocker: omits non-fatal exception guarantee.
  - why: Current @return already states 'a `Success` wrapping the result of applying `pf` to the exception ... any non-fatal exception thrown by `pf` is caught and returned as a `Failure`'.
- L333 `Failure#recoverWith` -> ruled for **neither**
  - accuracy: Blocker: clarify pf returns a Try.
  - style: Blocker: omits non-fatal exception guarantee.
  - why: Current @return already states 'the `Try` produced by applying `pf` ... any non-fatal exception thrown by `pf` is caught and returned as a `Failure`'.
- L405 `Success#flatMap` -> ruled for **neither**
  - accuracy: 
  - style: Blocker: omits non-fatal exception guarantee.
  - why: Current @return already states 'any non-fatal exception thrown by `f` is caught and returned as a `Failure`'.
- L417 `Success#transform` -> ruled for **neither**
  - accuracy: 
  - style: Blocker: omits non-fatal exception guarantee.
  - why: Current @return already states 'any non-fatal exception thrown by `s` is caught and returned as a `Failure`'.
- L425 `Success#map` -> ruled for **neither**
  - accuracy: Blocker: should say 'a Try containing' not 'a Success containing'.
  - style: Blocker: omits non-fatal exception guarantee.
  - why: Return type is Try[U] via Try[U](f(value)); the doc says 'a `Success` containing the result' which is the normal outcome, and the @return already includes the non-fatal Failure case.
- L421 `Success#collect` -> ruled for **neither**
  - accuracy: Blocker: should say 'a Try containing' and add message detail; also a nit about NoSuchElementException message.
  - style: Blocker: omits non-fatal exception guarantee.
  - why: Current @return already includes the NoSuchElementException case and the non-fatal exception guarantee; the value-in-message detail is not load-bearing.
- L429 `Success#filter` -> ruled for **neither**
  - accuracy: Nit: mention NoSuchElementException includes value in message.
  - style: Blocker: omits non-fatal exception guarantee.
  - why: Current @return already states the NoSuchElementException case and 'any non-fatal exception thrown by `p` is caught and returned as a `Failure`'; the message detail is not load-bearing.
- L433 `Success#fold` -> ruled for **neither**
  - accuracy: Blocker: clarify fa is applied to the exception thrown by fb.
  - style: Blocker: omits that fb exceptions are caught and passed to fa.
  - why: Current @return already states 'the result of applying `fb` to the value; any non-fatal exception thrown by `fb` is caught and passed to `fa`, whose result is then returned'.

## Outstanding worklist at the end

- L165 `Try.WithFilter#map` [nit/both]: The current doc already states the NoSuchElementException case; tighten wording so the 'wraps in a Try' phrasing does not obscure the Failure path.
- L177 `Try.WithFilter#foreach` [nit/both]: Add that `f` is a no-op when the predicate `p` is not satisfied.
- L341 `Failure#failed` [nit/both]: Rephrase to 'Inverts this `Failure` by returning a `Success` containing the exception.'
- L441 `Success#failed` [nit/both]: Rephrase to 'Inverts this `Success` by returning a `Failure` with an `UnsupportedOperationException`.'
- L186 `WithFilter.withFilter` [nit/style]: Shorten to 'Returns a new `WithFilter` combining the predicates `p` and `q`.'
- L295 `Failure.getOrElse` [nit/style]: Note in the `@param` or description that `default` is evaluated lazily (by-name).

## Inline NEEDS-HUMAN markers left in source
(none)
