# Doc review digest: library/src/scala/util/control/NonLocalReturns.scala

- models: writer devstral-2512 | accuracy mistral-medium-2508 | style mistral-large-2512 | adjudicator devstral-2512
- converged: false (up to 2 rounds)
- final refinement after review limit: true (not re-reviewed)
- accuracy verdict: approve
- style verdict: revise
- ADJUDICATOR verdict (final): revise

## Reviewer disagreements the adjudicator settled

- L27 `ReturnThrowable` -> ruled for **style**
  - accuracy: 
  - style: Rewrite the first sentence to clarify its purpose and scope: "An internal throwable used to implement nonlocal returns. This class is not intended for direct use by application code."
  - why: The style reviewer's suggestion clarifies the purpose and scope of the class, which is not addressed by the accuracy reviewer.
- L33 `ReturnThrowable.throwReturn` -> ruled for **style**
  - accuracy: 
  - style: Rewrite the description to accurately reflect the behavior: "Stores the given result and throws this exception to perform a nonlocal return. Always throws; never returns normally."
  - why: The style reviewer's suggestion accurately describes the behavior and clarifies that the method always throws an exception.
- L38 `ReturnThrowable.result` -> ruled for **style**
  - accuracy: 
  - style: Clarify the behavior and constraints: "Returns the result stored by a prior call to `throwReturn`. Throws an exception if called before `throwReturn` has been invoked."
  - why: The style reviewer's suggestion clarifies the behavior and constraints of the method, which is not addressed by the accuracy reviewer.

## Outstanding worklist at the end

- L27 `ReturnThrowable` [blocker/style]: Rewrite the first sentence to clarify its purpose and scope: "An internal throwable used to implement nonlocal returns. This class is not intended for direct use by application code."
- L33 `ReturnThrowable.throwReturn` [blocker/style]: Rewrite the description to accurately reflect the behavior: "Stores the given result and throws this exception to perform a nonlocal return. Always throws; never returns normally."
- L38 `ReturnThrowable.result` [blocker/style]: Clarify the behavior and constraints: "Returns the result stored by a prior call to `throwReturn`. Throws an exception if called before `throwReturn` has been invoked."
- L27 `ReturnThrowable` [nit/style]: Expand the `@tparam` description to match the style of the file: "@tparam T the type of the value that will be returned by the nonlocal return."
- L33 `ReturnThrowable.throwReturn` [nit/style]: Rewrite the `@param` description: "@param result the value to be stored and returned by the nonlocal return."

## Inline NEEDS-HUMAN markers left in source
(none)
