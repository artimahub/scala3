# Doc review digest: library/src/scala/Console.scala

- converged: false (after up to 2 rounds)
- final refinement after review limit: true (not re-reviewed)
- Codex verdict (accuracy emphasis): approve
- Claude verdict (style emphasis): revise

## Needs human / low confidence (check these first)

- L139 `Console.setOutDirect` [blocker/medium]: `@param out the new output stream returned by \`out\`` is circular/backwards: it reads as if the argument should be obtained from calling the `out` getter, when actually the reverse holds — after calling `setOutDirect`, the getter `out` will subsequently return whatever stream is passed in. A reader skimming just the @param line can plausibly misread the direction. → Describe the parameter directly, e.g. "the new output stream to install as the default", matching the plain style used at line 182 (`@param out the new output stream.`).
- L148 `Console.setErrDirect` [blocker/medium]: Same circular/backwards construction as setOutDirect's @param: `@param err the new error stream returned by \`err\`` implies the value comes from the `err` getter rather than stating that the getter will return it afterward. → Use plain phrasing consistent with line 213/227's `@param err the new error stream.`
- L157 `Console.setInDirect` [blocker/medium]: Same circular/backwards construction: `@param in the new input reader returned by \`in\`` implies the value comes from the `in` getter rather than stating that the getter will return it afterward. → Use plain phrasing consistent with `withIn`'s `@param reader the new input reader to use as the default input source` (line 249).

## Inline NEEDS-HUMAN markers left in source
(none)
