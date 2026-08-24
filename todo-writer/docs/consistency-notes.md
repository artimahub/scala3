# Notational inconsistencies observed, for a later pull request

Documentation inconsistencies noticed while writing, which are deliberately NOT
fixed in the documentation pull requests. Those PRs add documentation where
there was none, and that property is what makes them reviewable; editing
pre-existing prose alongside dilutes it.

Record what you find here instead. Each entry should let someone act on it
without repeating your search.

Format:

## Short description of the inconsistency

- **Predominant form:** what most of the library does, with a count if you have one
- **Divergent form:** what the minority does
- **Where:** files and lines, or a grep that finds them
- **Scale:** how many places are affected
- **Note:** anything that makes it less clear-cut than it looks

---

<!-- entries below -->

## Wrong or misleading pre-existing tags and prose (candidates for a cleanup PR)

- **Where:** `library/src/scala/Predef.scala:937` - `@tparam B the type of the left-hand side of the arrow association` on the `->` extension; `B` is the right-hand side. Same block (934-940) has tags but no summary sentence.
- **Where:** `library/src/scala/IArray.scala:967, :985` - `WithFilter.map`/`flatMap` docs say "all elements of this array" though only predicate-satisfying elements are mapped.
- **Where:** `library/src/scala/Function0.scala:35` - `apply()` doc says "to the arguments" at arity 0. Also `Function0/1/2` trait docs lack `@tparam` while `Function3`-`22` have them.
- **Where:** `library/src/scala/sys/process/ProcessBuilder.scala` ~lines 180-337 - "if the given capacity of lines **if** filled" typo in all eight capacity-overload docs; `lazyLines_!` overloads have `@param` but no `@return`; `process/package.scala` ~80 references nonexistent `#!!` (presumably `#||`).
- **Where:** `library/src/scala/util/control/Exception.scala` ~367 - `/** Convenience methods. */` renders as `toOption`'s own doc; `catching()` doc references nonexistent type "ControlExceptions" (is `ControlThrowable`).
- **Where:** `library/src/scala/quoted/Expr.scala` - `ofSeq`/`ofList` say "construct a copy of this sequence/list" (object methods; no copy). `quoted/runtime/QuoteMatching.scala` - "an the scrutineeExpr" typo, trailing double backtick. `quoted/runtime/Patterns.scala` - three holes share one summary; `patternType` summary garbled. `Quotes.scala` - `Super.apply` unbalanced bracket/stale `Id`; `SummonFrom` docs use old `given match` syntax; `Refined.apply` "represesenting" + empty `@return`; `AppliedTypeModule.apply` "Applied the..."; `TypeLambdaMethods.paramBounds`/`paramVariances` say "i-th parameter" for List-returning members.
- **Where:** `library/src/scala/collection/mutable/ListMap.scala:22` - class doc claims insertion order is preserved; `addOne`'s remove-accumulator reverses the traversed prefix (adding a,b,c yields iteration order c,a,b). Deprecated class; doc is false either way.
- **Where:** `library/src/scala/collection/Iterator.scala` - `range` "Creates nn iterator" (explicit-nulls migration typo); `fill`/`tabulate` `@return` reference `n` but params are `len`/`end`; `next()` "advance"; `contains` "is is". `Factory.scala:164, :934` - "a sequence of increasing of integers"; `Factory.fromSpecific` has tags but no summary.
- **Where:** `library/src/scala/collection/Seq.scala` ~311 - `$ccoll` macro (undefined; also in `Set.scala` `concat`); `appended` example has a stray `*` on the closing fence; `Set.scala` "union between of set", equals "for every `element` this set"; `@deprecated` message on varargs `-` references nonexistent `&-`.
- **Where:** `library/src/scala/collection/Map.scala` - `values` says "Collects ... in an iterable collection" but returns a lazy view (neighbours are careful about this); `MapView.scala:55` `filterKeys` `@return` says "an immutable map" for a view.
- **Where:** `library/src/scala/collection/IterableOnce.scala` ~786 - `hasDefiniteSize` doc contains a literal `@deprecated` tag and `**true**`-in-backticks markup; `copyToArray` `@param start` still says "of xs" after the rename to `dest`; `Iterable.scala` `newSpecificBuilder` has `@return` mid-sentence.
- **Where:** `library/src/scala/collection/immutable/Map.scala` - `removedAll` `@return` uses sequence "occurrence" language and references `elems` (param is `keys`); `removed` uses `*key*` emphasis; `transform` opens "This function transforms" with an in-place-sounding `@return`.
- **Where:** `library/src/scala/collection/immutable/TreeMap.scala:32-34, :64` - stray `* ` before links in class doc; `@param ordering ... type A` (key type is `K`). `TreeSet.excl` `@param` copied from `incl`; `incl`/`excl` say "Creates a new TreeSet" but return `this` when unchanged. `immutable/HashMap.scala:35` - `@tparam K ... in this hash set`.
- **Where:** `library/src/scala/collection/mutable/StringBuilder.scala` ~695 - `lastIndexOf(str, fromIndex)` `@param fromIndex` copied from `indexOf` (direction reversed); `substring` siblings declare different exception classes.
- **Where:** `library/src/scala/collection/mutable/ArrayDeque.scala` - class doc claims "amortized constant time" for random access (plain constant); `removeHeadOption`/`removeLastOption` have empty descriptions and bare `@return`; `clearAndShrink` (also `ArrayBuffer`) has `@param size` used inline in prose.
- **Where:** `library/src/scala/runtime/RichDouble.scala:141-152`, `RichFloat.scala:169-181` - `toRadians`/`toDegrees` `@return` mention a nonexistent parameter `x`.
- **Where:** `library/src/scala/concurrent/duration/Duration.scala` - abstract `*` doc's "semantics match Double" claim contradicted by `FiniteDuration.*` zero-times-infinity behaviour; `Duration.apply(Long, TimeUnit)` lacks the `@throws` its siblings now document.
- **Where:** cross-variant (JVM vs JS) wording divergences noted in `reflect/Manifest` factory tags, `Buffer` `@define coll` absence in the JS trait, `Enumeration.fromBitMask` "zero-adjusted" phrasing, `Range`/`NumericRange` factory param phrasing and example fences.
- **Note:** the full per-file report set (including minor grammar: "in a iterable", "advance the stepper", "ascendent", "used by for", "mutablity") was collected during the weeks 3-11 documentation run; this list keeps the items most likely to justify a cleanup PR.
- **Where:** `library/src/scala/collection/immutable/TreeSeqMap.scala` (`Ordering.modifyOrRemove`, pre-existing) - `@tparam S The type of the values in the resulting `LongMap`.` names the wrong class (copied from `LongMap`); the result is an `Ordering`.
- **Where:** `library/src/scala/collection/immutable/Queue.scala` ~55 - `apply(n)`'s pre-existing `@throws NoSuchElementException if the queue is too short`, but the body's `indexOutOfRange()` throws `IndexOutOfBoundsException`; the same block's `@return` sentence duplicates the summary. `iterator` just above reads "Returns the elements in the list as an iterator" on a queue.
- **Where:** `library/src/scala/collection/mutable/LinkedHashMap.scala` (`LinkedKeySet`, pre-existing) - `/** Note that a LinkedKeySet could be strict. */` sits BELOW the `@deprecated` annotation, so the parser drops it and the class renders undocumented. Unchanged since before this project; found by the annotation audit of 2026-08-24.
- **Where:** `library/src/scala/sys/Prop.scala` ~66-77 (pre-existing) - the doc block for the commented-out `//def or[T1 >: T](alt: => T1)` is followed by another doc comment for `clear()`, so the first block is orphaned (two doc comments, one declaration).
- **Where:** `library/src/scala/collection/immutable/Vector.scala` (`Vector.last`) - the `NoSuchElementException` thrown for an empty vector carries the message `"empty.tail"`; `head` just above uses `"empty.head"`, so `"empty.last"` looks intended. Found during the weeks 3-12 branch adjudication of 2026-08-24; the documented contract (the exception type) is unaffected, so the branch documents only that.
