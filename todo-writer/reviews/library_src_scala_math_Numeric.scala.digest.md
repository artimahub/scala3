# Doc review digest: library/src/scala/math/Numeric.scala

- models: writer devstral-latest | accuracy sonnet | style mistral-large-latest | adjudicator devstral-latest
- converged: false (up to 3 rounds)
- final refinement after review limit: true
- verification review of that refine: revise
- a real accuracy review ran at some point: true
- the file AS IT NOW STANDS was accuracy-reviewed: true
- accuracy verdict: revise
- style verdict: approve
- ADJUDICATOR verdict (final): revise

> **NOT REVIEWED.** Do not put this file in a PR on the strength of this
> digest. Read the diff yourself, or re-run the file once the reviewer is
> healthy. See reviews/NOT-REVIEWED.txt.

## Reviewer disagreements the adjudicator settled

- L997 `Numeric.signum` -> ruled for **style**
  - accuracy: Remove the `@deprecated Use \`sign\` instead.` line from the doc comment; the annotation already renders it.
  - style: Remove the prose restatement of the deprecation: "Returns the signum of a value of type `T`."
  - why: The accuracy reviewer's suggestion is more specific and aligns with the house rule against restating `@deprecated` in prose.
- L1045 `Numeric.NumericOps.signum` -> ruled for **accuracy**
  - accuracy: Drop the line '@deprecated("use \`sign\` method instead", since = "2.13.0")' entirely; the real `@deprecated` annotation on the `def` already renders the deprecation notice.
  - style: Remove the prose restatement of the deprecation: "Returns the signum of this value."
  - why: The accuracy reviewer correctly identifies that the line is malformed/broken scaladoc and violates the house rule against restating `@deprecated` in prose.
- L78 `Repeated @throws java.lang.ArithmeticException if `y` is zero` -> ruled for **style**
  - accuracy: 
  - style: Remove the `@throws` tag for `div` in `FloatIsFractional` and `DoubleIsFractional` (lines 685, 774), as floating-point division by zero does not throw an exception.
  - why: The style reviewer correctly identifies that the `@throws` tag is not relevant for `div` in `FloatIsFractional` and `DoubleIsFractional`.

## Outstanding worklist at the end

- L184 `IntIsIntegral.fromInt / IntIsIntegral.toInt / LongIsIntegral.toLong` [blocker/accuracy]: Change the @param wording for these three identity methods to something like 'the `Int` value (returned unchanged)', matching the pattern already used for FloatIsFractional.toFloat and DoubleIsFractional.toDouble.
- L1045 `Numeric.NumericOps.signum` [blocker/accuracy]: Drop the line '@deprecated("use \`sign\` method instead", since = "2.13.0")' entirely; the real `@deprecated` annotation on the `def` already renders the deprecation notice.
- L78 `Repeated @throws java.lang.ArithmeticException if `y` is zero` [nit/style]: Remove the `@throws` tag for `div` in `FloatIsFractional` and `DoubleIsFractional` (lines 685, 774), as floating-point division by zero does not throw an exception.
- L473 `CharIsIntegral.parseString` [nit/style]: Simplify the @return tag to: "@return `Some(Char)` if the string is a valid integer (truncated to `Char` via narrowing), `None` otherwise".
- L796 `BigDecimalIsConflicted.plus` [nit/style]: Clarify the @note tag to: "@note Returns `y` directly if `x` is the cached zero instance to avoid math context pollution, which may affect precision or rounding behavior."
- L807 `BigDecimalIsConflicted.minus` [nit/style]: Clarify the @note tag to: "@note Returns `-y` directly if `x` is the cached zero instance to avoid math context pollution."
- L819 `BigDecimalIsConflicted.times` [nit/style]: Clarify the @note tag to: "@note Returns `y` directly if `x` is the cached one instance to avoid math context pollution."
- L997 `Numeric.signum` [nit/style]: Remove the prose restatement of the deprecation: "Returns the signum of a value of type `T`."
- L1042 `Numeric.NumericOps.signum` [nit/style]: Remove the prose restatement of the deprecation: "Returns the signum of this value."
- L514 `CharIsIntegral.sign` [nit/accuracy]: Drop the trailing '(\u0000)', e.g. '\u0001 if `x` is nonzero, \u0000 if `x` is zero — `Char` values are unsigned.'
- L699 `FloatIsFractional.sign / DoubleIsFractional.sign` [nit/accuracy]: Note that the sign of zero is preserved, e.g. '0.0 if `x` is positive zero, -0.0 if `x` is negative zero'.

## Inline NEEDS-HUMAN markers left in source
(none)
