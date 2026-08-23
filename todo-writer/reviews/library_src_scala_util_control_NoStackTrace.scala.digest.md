# Doc review digest: library/src/scala/util/control/NoStackTrace.scala

- models: writer devstral-2512 | accuracy mistral-medium-2508 | style mistral-large-2512 | adjudicator devstral-2512
- converged: false (up to 2 rounds)
- final refinement after review limit: true (not re-reviewed)
- accuracy verdict: approve
- style verdict: revise
- ADJUDICATOR verdict (final): revise

## Reviewer disagreements the adjudicator settled

- L26 `NoStackTrace.fillInStackTrace` -> ruled for **style**
  - accuracy: 
  - style: The documentation claims the method 'optionally suppresses stack traces for efficiency', but the implementation does not make suppression optional at runtime. Suppression is controlled by the static `noSuppression` flag, not by any parameter or instance state. The description is misleading about how suppression is determined.
  - why: The accuracy reviewer did not raise any issues, while the style reviewer identified a significant inaccuracy in the documentation.
- L26 `NoStackTrace.fillInStackTrace` -> ruled for **style**
  - accuracy: 
  - style: The `@return` tag incorrectly states the method returns 'this `Throwable` instance without filling in the stack trace if suppression is enabled'. The code shows suppression is enabled when `noSuppression` is `false`, not `true`. The logic is inverted in the documentation.
  - why: The accuracy reviewer did not raise any issues, while the style reviewer identified a significant inaccuracy in the documentation.
- L33 `NoStackTrace.noSuppression` -> ruled for **style**
  - accuracy: 
  - style: The documentation does not mention that `noSuppression` is a `final def` that reads a mutable static field (`_noSuppression`). This is a material detail for users who might expect it to be a stable value or a configuration point. The lack of this information could lead to incorrect assumptions about thread safety or mutability.
  - why: The accuracy reviewer did not raise any issues, while the style reviewer identified a significant omission in the documentation.

## Outstanding worklist at the end

- L26 `NoStackTrace.fillInStackTrace` [blocker/style]: Revise the description to accurately reflect the behavior: 'Overrides the default stack trace filling behavior to suppress stack traces if `NoStackTrace.noSuppression` is `false`, otherwise delegates to the superclass implementation. Suppression is controlled globally by the `noSuppression` flag, not per instance.'
- L26 `NoStackTrace.fillInStackTrace` [blocker/style]: Correct the `@return` tag to: '@return this `Throwable` instance without filling in the stack trace if `noSuppression` is `false`, otherwise the result of the superclass implementation.'
- L33 `NoStackTrace.noSuppression` [blocker/style]: Add a `@note` tag to clarify: '@note This method reads a mutable static field. The returned value may change if the field is modified elsewhere in the application.'
- L26 `NoStackTrace.fillInStackTrace` [nit/style]: Revise the first sentence to: 'Overrides `fillInStackTrace` to conditionally suppress stack trace generation based on the global `noSuppression` setting.'
- L33 `NoStackTrace.noSuppression` [nit/style]: Revise the description to provide unique value, such as: 'Indicates the global setting for stack trace suppression in `NoStackTrace`.'

## Inline NEEDS-HUMAN markers left in source
(none)
