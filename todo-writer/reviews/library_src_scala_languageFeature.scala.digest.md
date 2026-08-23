# Doc review digest: library/src/scala/languageFeature.scala

- converged: false (after up to 2 rounds)
- final refinement after review limit: true (not re-reviewed)
- Codex verdict (accuracy emphasis): revise
- Claude verdict (style emphasis): 

## Needs human / low confidence (check these first)

- L32 `languageFeature.reflectiveCalls` [blocker/high]: `reflectiveCalls` is listed by the compiler as a legacy Scala 2 feature that is no longer supported; no compiler behavior consults this feature. It therefore does not permit reflective structural-member access in Scala 3. → Describe it as a legacy Scala 2 compatibility witness with no effect in Scala 3, rather than as permitting reflective access.
- L43 `languageFeature.higherKinds` [blocker/high]: Higher-kinded types no longer require this language import, and the compiler lists `higherKinds` among unsupported legacy features. Saying the feature permits writing them incorrectly implies an effective opt-in. → Describe it as a deprecated/legacy compatibility witness and state that higher-kinded types do not require a language import in Scala 3.

## Inline NEEDS-HUMAN markers left in source
(none)
