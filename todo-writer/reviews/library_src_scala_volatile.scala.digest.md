# Doc review digest: library/src/scala/volatile.scala

- converged: false (after up to 2 rounds)
- final refinement after review limit: true (not re-reviewed)
- Codex verdict (accuracy emphasis): revise
- Claude verdict (style emphasis): approve

## Needs human / low confidence (check these first)

- L20 `volatile` [blocker/high]: The claim that it marks a field volatile and gives its reads and writes volatile semantics is not true for every permitted application: the compiler accepts `@volatile` on a `val` but emits `values cannot be volatile`; a final JVM field cannot be volatile. → Limit the contract to mutable fields, e.g. “Marks a mutable field as volatile, giving its reads and writes the JVM’s volatile-field visibility and ordering semantics.”

## Inline NEEDS-HUMAN markers left in source
(none)
