# Doc review digest: library/src/scala/PartialFunction.scala

- converged: false (after up to 2 rounds)
- final refinement after review limit: true (not re-reviewed)
- Codex verdict (accuracy emphasis): revise
- Claude verdict (style emphasis): 

## Needs human / low confidence (check these first)

- L501 `PartialFunction.Unlifted.isDefinedAt` [blocker/high]: The `@return` says `false` otherwise, but `f(x).isDefined` throws `NullPointerException` when `f(x)` returns `null`; null is possible for an `A => Option[B]` on the JVM. → State the non-null requirement for `f(x)`, or document that a null result causes `NullPointerException` rather than returning `false`.
- L512 `PartialFunction.Unlifted.applyOrElse` [blocker/high]: The documentation omits that `f(x).getOrElse(...)` throws `NullPointerException` if `f(x)` returns `null`, rather than applying `default`. → State the non-null requirement for `f(x)`, or document the `NullPointerException` behavior for a null result.

## Inline NEEDS-HUMAN markers left in source
(none)
