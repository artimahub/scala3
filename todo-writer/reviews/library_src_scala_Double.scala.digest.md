# Doc review digest: library/src/scala/Double.scala

- converged: false (after up to 2 rounds)
- accuracy verdict: revise
- style verdict: approve

## Needs human / low confidence (check these first)

- L26 `Double.toByte` [blocker/high]: The documented first step only accounts for NaN and finite out-of-Int-range values before saying values are otherwise rounded toward zero. `Double.PositiveInfinity.toInt` and `Double.NegativeInfinity.toInt` instead saturate to `Int.MaxValue`/`Int.MinValue`, and then narrow to `Byte`. Infinity is a material Double edge case not covered by the stated conversion rule. → Mention infinities together with out-of-range values, e.g. non-NaN values whose rounded/truncated result is outside the `Int` range, including infinities, saturate to `Int.MinValue` or `Int.MaxValue` before narrowing.
- L28 `Double.toShort` [blocker/high]: Same issue as `toByte`: the documented initial `Int` conversion omits infinities, which actually saturate to `Int.MaxValue`/`Int.MinValue` before narrowing to `Short`, rather than being rounded toward zero. → Include `Double.PositiveInfinity` and `Double.NegativeInfinity` in the saturation behavior of the initial `Int` conversion.
- L30 `Double.toChar` [blocker/high]: Same issue as `toByte`: infinities are not finite out-of-range values and are not rounded toward zero; they first convert to saturated `Int` values and are then narrowed to `Char`. The current wording leaves that material edge case misdescribed. → Include infinities in the saturation behavior of the initial `Int` conversion before the low-order-16-bit narrowing.
- L32 `Double.toInt` [blocker/high]: The comment says the value is returned as an `Int`, rounded toward zero, with special cases for NaN and finite out-of-range values. It omits infinities, which convert to `Int.MaxValue` or `Int.MinValue`, not by rounding toward zero. → Document that positive and negative infinity also convert to `Int.MaxValue` and `Int.MinValue`, respectively.
- L34 `Double.toLong` [blocker/high]: The comment says the value is returned as a `Long`, rounded toward zero, with special cases for NaN and finite out-of-range values. It omits infinities, which convert to `Long.MaxValue` or `Long.MinValue`, not by rounding toward zero. → Document that positive and negative infinity also convert to `Long.MaxValue` and `Long.MinValue`, respectively.

## Inline NEEDS-HUMAN markers left in source
(none)
