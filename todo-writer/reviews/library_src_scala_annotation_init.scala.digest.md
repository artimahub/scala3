# Doc review digest: library/src/scala/annotation/init.scala

- converged: false (after up to 2 rounds)
- accuracy verdict: revise
- style verdict: approve

## Needs human / low confidence (check these first)

- L40 `init.widen` [blocker/medium]: The added Scaladoc says `@widen(height)` widens the annotated argument's abstract value to the specified maximum height, but the implementation is only an empty `StaticAnnotation`, and the initialization checker references `InitWidenAnnot` only in `Definitions` with no apparent use. The current argument widening paths use fixed/default widening and do not read this annotation or its `height`. → Either implement/use `scala.annotation.init.widen` in the checker, or change the Scaladoc so it does not claim active checker behavior controlled by `height`.

## Inline NEEDS-HUMAN markers left in source
(none)
