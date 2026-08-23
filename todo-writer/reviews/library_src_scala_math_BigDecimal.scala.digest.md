# Doc review digest: library/src/scala/math/BigDecimal.scala

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

- L776 `BigDecimal.setScale(scale: Int, mode: RoundingMode)` -> ruled for **style**
  - accuracy: Drop the @return tag and add @throws java.lang.NullPointerException if `mode` is `null`.
  - style: Change the @throws tag to: "@throws java.lang.ArithmeticException if `mode` is `UNNECESSARY` and rounding is required for the requested `scale`."
  - why: The @return tag is redundant, but the NullPointerException tag is not necessary as the parameter is not nullable in the context.

## Outstanding worklist at the end

- L477 `BigDecimal.this(bigDecimal: BigDec)` [blocker/accuracy]: Add `@throws java.lang.IllegalArgumentException if `bigDecimal` is `null` to match the behavior documented in `BigDecimal.apply(bd: BigDec)`.
- L59 `BigDecimal.RoundingMode.UNNECESSARY` [nit/style]: Change the `@note` to: "@note This mode throws an `ArithmeticException` if rounding is required."
- L122 `BigDecimal.decimal(bd: BigDec, mc: MathContext)` [nit/style]: Change the `@throws` tag to: "@throws java.lang.NullPointerException if `bd` is `null`."
- L309 `BigDecimal.apply(x: Array[Char])` [nit/style]: Change the `@throws` tag to: "@throws java.lang.NullPointerException if `x` is `null`."
- L328 `BigDecimal.apply(x: String)` [nit/style]: Change the `@throws` tag to: "@throws java.lang.NullPointerException if `x` is `null`."
- L347 `BigDecimal.apply(x: BigInt)` [nit/style]: Change the `@throws` tag to: "@throws java.lang.NullPointerException if `x` is `null`."
- L386 `BigDecimal.apply(bd: BigDec)` [nit/style]: Change the `@throws` tag to: "@throws java.lang.IllegalArgumentException if `bd` is `null`."
- L477 `BigDecimal.this(bigDecimal: BigDec)` [nit/style]: Change the `@param` tag to: "@param bigDecimal the underlying `java.math.BigDecimal`."
- L776 `BigDecimal.setScale(scale: Int, mode: RoundingMode)` [nit/style]: Change the `@throws` tag to: "@throws java.lang.ArithmeticException if `mode` is `UNNECESSARY` and rounding is required for the requested `scale`."

## Inline NEEDS-HUMAN markers left in source
(none)
