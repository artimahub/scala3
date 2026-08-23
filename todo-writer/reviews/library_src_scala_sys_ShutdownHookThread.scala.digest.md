# Doc review digest: library/src/scala/sys/ShutdownHookThread.scala

- converged: false (after up to 2 rounds)
- final refinement after review limit: true (not re-reviewed)
- Codex verdict (accuracy emphasis): revise
- Claude verdict (style emphasis): revise

## Needs human / low confidence (check these first)

- L28 `ShutdownHookThread.remove` [blocker/high]: The documentation mentions `IllegalStateException` but omits `SecurityException`, which `Runtime.removeShutdownHook` can throw when the caller is denied the `RuntimePermission("shutdownHooks")` permission. → Add a `@throws SecurityException` entry (or equivalent prose) describing the permission-denied case.
- Lnull `null` [blocker/high]: null → 

## Inline NEEDS-HUMAN markers left in source
(none)
