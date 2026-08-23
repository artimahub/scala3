# Doc review digest: library/src/scala/math/ScalaNumericConversions.scala

- models: writer devstral-latest | accuracy sonnet | style mistral-large-latest | adjudicator devstral-latest
- converged: false (up to 3 rounds)
- final refinement after review limit: true
- verification review of that refine: revise
- a real accuracy review ran at some point: true
- the file AS IT NOW STANDS was accuracy-reviewed: true
- accuracy verdict: revise
- style verdict: revise
- ADJUDICATOR verdict (final): revise

> **NOT REVIEWED.** Do not put this file in a PR on the strength of this
> digest. Read the diff yourself, or re-run the file once the reviewer is
> healthy. See reviews/NOT-REVIEWED.txt.

## Reviewer disagreements the adjudicator settled

- L39 `intValue / longValue (shared block with byteValue, shortValue at lines 35, 37, 39, 41)` -> ruled for **merged**
  - accuracy: The added doc claims out-of-range values are 'narrowed to the low-order bits of the target type and can lose magnitude or flip sign' for all four methods. That is true of the BigInt/BigDecimal implementations (java.math.BigInteger/BigDecimal.intValue()/longValue() docs literally say 'low-order bits ... can ... return a result with the opposite sign') and of RichInt/RichLong/RichChar/RichByte/RichShort (integral-to-integral two's-complement narrowing). But RichDouble.intValue/longValue and RichFloat.intValue/longValue are implemented as `self.toInt`/`self.toLong` (library/src/scala/runtime/RichDouble.scala:25-26, RichFloat.scala:25-26), which is a floating-point-to-integral narrowing conversion per JLS 5.1.3: it SATURATES to Int/Long MinValue/MaxValue for out-of-range magnitudes and maps NaN to 0. It never takes 'low-order bits' of anything and sign never flips (a huge positive double clamps to Int.MaxValue, not to some sign-flipped garbage value). Since Double and Float are real conformers of this trait, a reader of intValue/longValue's Scaladoc is told the wrong mechanism and a guarantee ('sign never flips under saturation') that is actually the opposite of what's documented. byteValue/shortValue are comparatively fine, since their underlying `.toByte`/`.toShort` conversions do end in a genuine low-order-bit truncation step even for Double/Float sources (JVM narrows via an intermediate int first, then truncates to the 8/16-bit target).
  - style: The doc states "Values that don't fit are narrowed to the low-order bits of the target type and can lose magnitude or flip sign." This is misleading for `Byte`: narrowing to 8 bits cannot flip the sign of a value that was already within `Byte` range, and the phrase "can lose magnitude" is vague. The implementation (not shown in the diff but implied by the pattern) truncates to 8 bits, which may change the value but not its sign unless the original value was outside `Byte` range.
  - why: Both reviewers identified inaccuracies in the narrowing behavior documentation, and their concerns were merged into a single instruction to clarify the behavior for all methods.
- L104 `unifiedPrimitiveHashcode` -> ruled for **style**
  - accuracy: 
  - style: The doc claims "Returns the [[scala.Int]] hash code if the value fits in an [[scala.Int]]; otherwise, returns the [[scala.Long]] hash code." This is false: the method returns an `Int` in both cases. When the value does not fit in an `Int`, it returns `lv.##`, which is the hash code of the `Long` (an `Int`). The doc should not claim it returns a `Long` hash code.
  - why: The style reviewer correctly identified a misleading statement in the documentation, which was not addressed by the accuracy reviewer.

## Outstanding worklist at the end

- L39 `intValue / longValue (shared block with byteValue, shortValue at lines 35, 37, 39, 41)` [blocker/both]: Split the wording: keep the low-order-bits/sign-flip language for byteValue and shortValue, but describe intValue/longValue's general contract without asserting bit-truncation, e.g., 'Values that don't fit are narrowed to the target type; the exact result (bit-truncated or saturated) depends on the implementation.'
- L23 `underlying` [blocker/style]: Add "or `null`" to the description: "Returns the underlying value as a Java `Object`, or `null`."
- L36 `byteValue` [blocker/style]: Clarify the behavior: "Returns the value of this number as a [[scala.Byte]]. If the value is outside the range of `Byte` (${Byte.MinValue} to ${Byte.MaxValue}), it is truncated to 8 bits, which may change its magnitude and sign."
- L38 `shortValue` [blocker/style]: Use the same clarification as `byteValue`, adjusted for `Short`: "Returns the value of this number as a [[scala.Short]]. If the value is outside the range of `Short` (${Short.MinValue} to ${Short.MaxValue}), it is truncated to 16 bits, which may change its magnitude and sign."
- L104 `unifiedPrimitiveHashcode` [blocker/style]: Correct the description: "Returns the [[scala.Int]] hash code of this value. If the value fits in an [[scala.Int]], it is returned directly; otherwise, the hash code of the [[scala.Long]] representation is returned."
- L40 `intValue` [nit/style]: Clarify: "Returns the value of this number as an [[scala.Int]]. If the value is outside the range of `Int` (${Int.MinValue} to ${Int.MaxValue}), it is truncated to 32 bits, which may change its magnitude and sign."
- L42 `longValue` [nit/style]: Clarify: "Returns the value of this number as a [[scala.Long]]. If the value is outside the range of `Long`, it may overflow or lose precision."

## Inline NEEDS-HUMAN markers left in source
(none)
