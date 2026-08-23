# Doc review digest: library/src/scala/StringContext.scala

- converged: false (after up to 2 rounds)
- final refinement after review limit: true (not re-reviewed)
- Codex verdict (accuracy emphasis): revise
- Claude verdict (style emphasis): 

## Needs human / low confidence (check these first)

- L178 `StringContext.standardInterpolator` [blocker/high]: The method delegates to the companion implementation, which throws `IllegalArgumentException` when `args` and this context's `parts` have incompatible lengths; the documentation does not state this material failure mode. → Add an `@throws IllegalArgumentException` tag explaining the required parts-to-arguments relationship.
- L387 `StringContext.treatEscapes` [blocker/high]: The delegated implementation can throw `InvalidEscapeException` for an invalid ordinary escape and `InvalidUnicodeEscapeException` for a malformed Unicode escape, but neither failure mode is documented. → Document both exceptions, or explicitly state that invalid ordinary and Unicode escape sequences raise their respective exception types.
- L404 `StringContext.processEscapes` [blocker/high]: The documentation says any backslash not starting a valid escape raises `InvalidEscapeException`, but malformed Unicode escapes such as `"\\u1"` reach `readUEscape` and raise `InvalidUnicodeEscapeException` instead. → Distinguish invalid ordinary escapes (`InvalidEscapeException`) from malformed Unicode escapes (`InvalidUnicodeEscapeException`) and document both.
- L417 `StringContext.processUnicode` [blocker/high]: Malformed Unicode escapes that are eligible for processing throw `InvalidUnicodeEscapeException`, but this material exception is undocumented. → Add an `@throws InvalidUnicodeEscapeException` tag for invalid eligible Unicode escape sequences.
- L516 `StringContext.standardInterpolator` [blocker/high]: Although the documentation states the required parts count, it does not say that the implementation enforces it by throwing `IllegalArgumentException`. → Add an `@throws IllegalArgumentException` tag for a parts count other than `args.length + 1`.

## Inline NEEDS-HUMAN markers left in source
(none)
