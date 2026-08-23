# Doc review digest: library/src/scala/ref/ReferenceQueue.scala

- converged: false (after up to 2 rounds)
- accuracy verdict: revise
- style verdict: approve

## Needs human / low confidence (check these first)

- L42 `ReferenceQueue.remove` [blocker/high]: The doc says this blocks until a reference becomes available, but the implementation delegates to java.lang.ref.ReferenceQueue.remove(), which can throw InterruptedException if interrupted before a reference is available. → Mention interruption, e.g. add @throws[InterruptedException] or say it blocks until a reference is available unless interrupted.
- L48 `ReferenceQueue.remove` [blocker/high]: The doc describes only availability, timeout, and IllegalArgumentException for negative timeouts. The implementation delegates to java.lang.ref.ReferenceQueue.remove(timeout), which can also throw InterruptedException if interrupted while waiting. → Add the interrupt case, e.g. @throws[InterruptedException] if interrupted while waiting.

## Inline NEEDS-HUMAN markers left in source
(none)
