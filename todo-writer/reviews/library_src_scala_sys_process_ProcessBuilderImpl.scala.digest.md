# Doc review digest: library/src/scala/sys/process/ProcessBuilderImpl.scala

- converged: false (after up to 2 rounds)
- final refinement after review limit: true (not re-reviewed)
- Codex verdict (accuracy emphasis): revise
- Claude verdict (style emphasis): 

## Needs human / low confidence (check these first)

- L257 `AbstractBuilder.lazyLines` [blocker/high]: `capacity` must be non-null and positive: it is passed to `LinkedBlockingQueue`, which throws `NullPointerException` for null and `IllegalArgumentException` for zero or negative values. The documentation presents it only as a maximum. → State in the `@param capacity` description that it must be positive (and non-null), and that invalid values fail when creating the queue.
- L265 `AbstractBuilder.lazyLines` [blocker/high]: `capacity` must be non-null and positive: it is passed to `LinkedBlockingQueue`, which throws `NullPointerException` for null and `IllegalArgumentException` for zero or negative values. The documentation presents it only as a maximum. → State in the `@param capacity` description that it must be positive (and non-null), and that invalid values fail when creating the queue.
- L274 `AbstractBuilder.lazyLines_!` [blocker/high]: `capacity` must be non-null and positive: it is passed to `LinkedBlockingQueue`, which throws `NullPointerException` for null and `IllegalArgumentException` for zero or negative values. The documentation presents it only as a maximum. → State in the `@param capacity` description that it must be positive (and non-null), and that invalid values fail when creating the queue.
- L280 `AbstractBuilder.lazyLines_!` [blocker/high]: `capacity` must be non-null and positive: it is passed to `LinkedBlockingQueue`, which throws `NullPointerException` for null and `IllegalArgumentException` for zero or negative values. The documentation presents it only as a maximum. → State in the `@param capacity` description that it must be positive (and non-null), and that invalid values fail when creating the queue.

## Inline NEEDS-HUMAN markers left in source
(none)
