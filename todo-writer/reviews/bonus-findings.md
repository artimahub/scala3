## library/src/scala/io/Position.scala
- L16 `Position`: The pre-existing class-level Scaladoc says "The object Position" even though the comment is attached to the private abstract class `Position`. → Either move object-specific wording to the companion object or describe the abstract class/companion relationship more precisely.
- L23 `Position`: The pre-existing Scaladoc says line numbers greater than `LINE_MASK` are replaced, but `encode` also clamps `line == LINE_MASK` and forces column to 0. → Say values at or above `LINE_MASK` are clamped/encoded with column 0.

## library/src/scala/io/BufferedSource.scala
- L21 `BufferedSource`: Pre-existing class Scaladoc says "This object" even though BufferedSource is a class. → Change the description to refer to a class or source instead of an object.

## library/src/scala/io/BufferedSource.scala
- L27 `BufferedSource`: Pre-existing class documentation says "This object" even though BufferedSource is a class. → Change "This object" to "This class".

## library/src/scala/Proxy.scala
- L17 `Proxy`: The pre-existing Scaladoc says "This class implements", but `Proxy` is a trait. → Change "class" to "trait".

## library/src/scala/annotation/MacroAnnotation.scala
- L234 `MacroAnnotation.transform`: Pre-existing @return text says additional new definitions follow the transformed definition, but the surrounding pre-existing description and examples allow additions before or after the transformed definition, preserving order. → Change the @return description to avoid imposing order, e.g. “a non-empty list containing the transformed definition, which must reuse the original symbol, and any additional new definitions.”

## library/src/scala/annotation/capability.scala
- L6 `capability`: The pre-existing enclosing comment says "If the annotation is present and -Ycc is set", but this repository’s current user-facing flag for capture checking appears to be `-language:experimental.captureChecking`/`import language.experimental.captureChecking`; `-Ycc` is no longer the visible option in the file context. → Consider updating the pre-existing comment to say "under capture checking" or name the current capture-checking enablement mechanism.

## library/src/scala/annotation/init.scala
- L5 `init`: Pre-existing Scaladoc says "static obects". → Fix typo to "static objects".

## library/src/scala/caps/package.scala
- L220 `freeze`: Pre-existing doc refers to a pure operation `op`, but `freeze` takes parameter `x` and returns `x.type`; there is no `op` parameter or operation wrapper in this declaration. → Rewrite the pre-existing description to describe freezing the argument `x`, not the result of `op`.
- L196 `internal.paramAlias`: Pre-existing `@param` tag names `parmName`, but the constructor parameter is `paramName`. → Change `@param parmName` to `@param paramName`.

## library/src/scala/languageFeature.scala
- L46 `language.dynamics`: Pre-existing documentation says any class, trait, or object with Dynamic as a base trait is rejected when the feature is not enabled, but the compiler only rejects direct Dynamic parents and emits a feature warning for indirect subclasses. → Match the new languageFeature.dynamics wording: direct subclasses are rejected; indirect subclasses trigger a feature warning.
- L134 `language.higherKinds`: Pre-existing documentation says higher-kinded types trigger a warning unless higherKinds is enabled, but in Scala 3 higher-kinded types no longer require a language import. → Update this Scaladoc to say the import is deprecated and no longer required.

## library/src/scala/specialized.scala
- L19 `tspecialized`: The pre-existing enclosing comment similarly says type parameters are automatically specialized, but it is attached only to a commented-out declaration and appears to describe unsupported general specialization behavior in Scala 3. → A human should either remove this stale commented-out documentation or update it to match the current limited specialization support.

## library/src/scala/languageFeature.scala
- L80 `scala.language.reflectiveCalls`: The pre-existing documentation says enabling this feature supports reflective structural access, but the compiler classifies it as an unsupported legacy Scala 2 feature. → Update it to describe legacy compatibility/no Scala 3 effect.
- L130 `scala.language.higherKinds`: The pre-existing documentation says disabled `higherKinds` triggers a warning, although higher-kinded types no longer require the import and the compiler classifies the feature as legacy. → Update it to state that the import is obsolete in Scala 3.
- L168 `scala.language.existentials`: The pre-existing documentation describes an active opt-in for existential types, while the compiler classifies `existentials` as an unsupported legacy Scala 2 feature. → Update it to describe only legacy compatibility, consistent with the new witness-type documentation.

## library/src/scala/typeConstraints.scala
- L65 `null`: null → null

## library/src/scala/Product1.scala
- Lnull `null`: null → null

## library/src/scala/sys/process/ProcessIO.scala
- L35 `ProcessIO`: The class documentation says `ProcessBuilder` calls all three handlers in separate threads and marks all of them daemon when requested. Simple processes can omit input/error handlers, and their input thread is always daemon. → Qualify the statement to describe the applicable handlers and document the simple-process input-thread exception.

## library/src/scala/sys/process/ProcessImpl.scala
- L305 `DummyProcess`: The pre-existing class comment incorrectly calls `DummyProcess` a wrapper around `java.lang.Process` and refers to nonexistent `ioThreads`. → Describe it as a process backed by a thread evaluating `action`, or remove the stale comment.

## library/src/scala/util/ChainingOps.scala
- L61 `null`: null → null
- L68 `null`: null → null

