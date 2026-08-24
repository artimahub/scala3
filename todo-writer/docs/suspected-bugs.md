# Suspected bugs found while documenting (weeks 3-12)

Each entry records what the code does, what the siblings or contract suggest
it should do, and the evidence. None are fixed in the documentation PRs
(comment-only).

Entries below say "NEEDS-HUMAN at the site" where the PR branch once carried
an `@note NEEDS-HUMAN:` there. Those notes are gone: adjudication of the
branch reviews on 2026-08-24 removed all 24 of them, because a `@note`
renders as a visible block in the published Scaladoc. Where the code answered
the question, the answer went into the doc in its place; otherwise the doc
says only what is unambiguously true and this file carries the question. This
file is now the only record.

## `library/src/scala/jdk/DoubleAccumulator.scala:561` and `LongAccumulator.scala:570` - `nextStep` exhaustion guard

Both steppers guard `if (n <= 0) throw new NoSuchElementException(...)`,
where `n` is the size of the currently loaded block. The siblings guard on
elements remaining: `AnyAccumulator.scala:439` and `IntAccumulator.scala:568`
use `if (N <= 0)`. After the last element of a nonempty accumulator is
consumed (`i == n`, `N == 0`), a further `nextStep` call does not throw: the
guard passes (`n > 0`), `loadMore()` reloads `acc.current` with `i = 0`, and
the call returns a stale element while `N` goes negative, corrupting
`estimateSize`. It throws only when the stepper was built from an empty
accumulator. Everything else about the four steppers is identical, so
`N <= 0` is almost certainly what was meant. NEEDS-HUMAN notes at both sites.

## `library/src/scala/collection/mutable/CollisionProofHashMap.scala:181` - `sizeHint` shadowing

`sizeHint(size: Int)` contains `if(size == 0) reallocTable(target)`, testing
the `size` **parameter**, which shadows the `size` method (contentSize). The
analogous branches in `HashMap.growTable` and `HashSet.growTable` test the
current content size. `reallocTable` replaces the table with a fresh empty
array without resetting `contentSize`, so with a small load factor (e.g.
1/32) a 1-element map can take this branch on `sizeHint(0)` and silently drop
every entry while `size` still reports 1. Intended is almost certainly
`contentSize == 0`. NEEDS-HUMAN at the site.

## `library/src/scala/collection/mutable/CollisionProofHashMap.scala:246` - `put` return value

`put0` ends with `if(res) Some(null.asInstanceOf[V]) else null //TODO` (the
`//TODO` is in the shipped source). `res` is true when a new entry was
inserted, so public `put` returns `Some(null)` where `MapOps.put`'s contract
says `None`; and on replacement inside a red-black-tree bucket,
`insert`/`insertIntoExisting` overwrite `x.value` and return `false` without
capturing the old value, so `put` returns `None` where the contract says
`Some(previousValue)`. Only replacement inside a linked-list bucket returns
the previous value correctly (early return at line ~229). Internal callers
all pass `getOld = false`, so only public `put` is affected. NEEDS-HUMAN at
the site.

## `library/src/scala/collection/LazyZipOps.scala:135` - `LazyZip2.filter` inverted `isEmpty`

The anonymous view returned by `LazyZip2.filter` overrides
`isEmpty: Boolean = iterator.hasNext` - the inverse of the contract. The
identical operation in `LazyZip3.filter` and `LazyZip4.filter` uses
`iterator.isEmpty`. User-visible when the `BuildFrom` result is the view
itself (e.g. zipping views): `xs.lazyZip(ys).filter(p).isEmpty` answers
backwards. The same line exists verbatim in upstream Scala 2.13.18.

## `library/src/scala/collection/LazyZipOps.scala:100` - `LazyZip2.flatMap` wrong `isEmpty`

The anonymous view overrides `isEmpty = coll1.isEmpty || coll2.isEmpty`.
When both inputs are non-empty but `f` returns only empty collections, the
view is empty yet `isEmpty` answers `false`. `LazyZip3`/`LazyZip4` flatMap
use `iterator.isEmpty`. Also identical in upstream 2.13.18.

## `library-js/src/scala/Enumeration.scala` (`ValueSet.iteratorFrom`) - missing `bottomId` adjustment

The JS implementation is `nnIds iteratorFrom start.id map (id =>
thisenum.apply(bottomId + id))`: the bit set stores zero-adjusted ids
(`id - bottomId`, see `incl`/`excl`/`contains` in the same class) but the
bound is not adjusted. The JVM counterpart
(`library/src/scala/Enumeration.scala:323-324`) subtracts:
`nnIds.iteratorFrom(start.id - bottomId)`. For an enumeration with negative
`initial` (`bottomId < 0`), `iteratorFrom(start)` wrongly includes values
below `start.id`. Correct when `bottomId == 0`, which is why it goes
unnoticed. Mirrors a Scala 2 bug fixed on the JVM side; the Scala.js
override never got the fix. NEEDS-HUMAN at the site.

## `library-js/src/scala/runtime/BoxesRunTime.scala:52` - `boxToByte` declared to return `java.lang.Boolean`

`def boxToByte(b: Byte): java.lang.Boolean = b.asInstanceOf[java.lang.Boolean]`.
Every other `boxTo*` returns the matching wrapper class, and upstream
Scala.js declares `boxToByte(b: Byte): java.lang.Byte`. Under compliant cast
semantics the method as written can never successfully box a `Byte`
(`ClassCastException` for every non-null value); likely harmless only if the
back-end never links it, but the declaration is wrong. Pre-exists on main.
NEEDS-HUMAN at the site.

## `library/src/scala/collection/immutable/ArraySeq.scala:138-172` - `appendedAllArraySeq` fast path can throw `ArrayStoreException`

The optimization guard only distinguishes object arrays from primitive
arrays (`isInstanceOf[Array[AnyRef]]` on both operands), aborting only when
they differ. When both operands are backed by primitive arrays of
*different* element types - reachable through a common supertype thanks to
covariance, e.g. `ArraySeq(1) ++ ArraySeq(1L)` typed at `AnyVal` - the
"primitive" branch allocates via the receiver's `ClassTag` (an `Array[Int]`)
and `System.arraycopy`s the other operand's `Array[Long]` into it, throwing
`ArrayStoreException`. The in-code comment "A is a primative and B = A"
states an assumption the guard does not enforce; the generic boxed fallback
handles the combination fine. Same code exists in Scala 2.13. NEEDS-HUMAN at
`appendedAll`/`prependedAll` (~lines 222/265).

## `library/src/scala/collection/mutable/UnrolledBuffer.scala:~245` - `remove(idx, count)` contract deviations

Implemented as `if (count > 0) { remove(idx); remove(idx, count-1) }`: a
negative `count` is a silent no-op (contract:
`@throws IllegalArgumentException if count < 0`), and when
`idx + count > length` it removes elements one at a time until a per-element
bounds check throws, leaving the buffer partially modified (contract: an
up-front bounds check, as `ArrayBuffer` and `ListBuffer` implement).
NEEDS-HUMAN at the site.

## `library/src/scala/collection/mutable/UnrolledBuffer.scala:~330` - `patchInPlace` does not clamp

Implemented as `remove(from, replaced); insertAll(from, patch)`; throws
`IndexOutOfBoundsException` (possibly after partial modification) for
out-of-range `from`/`replaced`, where the `Buffer.patchInPlace` contract
documents clamping (negative indices as 0, past-the-end as append), which
`ListBuffer` and `IndexedBuffer` honor. Example:
`buf.patchInPlace(buf.length + 1, p, 0)` throws where the contract promises
an append. NEEDS-HUMAN at the site.

## `library/src/scala/collection/mutable/OpenHashMap.scala:349-354` - exhausted iterator throws the wrong exception

`OpenHashMapIterator.next()` evaluates `table(index)` unconditionally; on an
exhausted iterator `advance()` leaves `index == mask + 1`, so `next()`
throws `ArrayIndexOutOfBoundsException` where the `Iterator.next()` contract
(and every sibling map iterator) requires `NoSuchElementException`. Class is
deprecated since 2.13.0. NEEDS-HUMAN at the site.

## `library/src/scala/collection/convert/StreamExtensions.scala:767, 775` - mistyped accumulator factory instances

`jLongAccumulatorFactoryInfo` is declared
`AccumulatorFactoryInfo[jl.Long, IntAccumulator]` and
`jDoubleAccumulatorFactoryInfo` `[jl.Double, IntAccumulator]`, casting the
long/double instances into those types. The sibling
`jIntegerAccumulatorFactoryInfo` is `[jl.Integer, IntAccumulator]`, so these
should be `[jl.Long, LongAccumulator]` and `[jl.Double, DoubleAccumulator]` -
a copy-paste error in the second type argument. Effect: no implicit
`AccumulatorFactoryInfo[jl.Long, LongAccumulator]` exists, so collecting a
`Stream[jl.Long]` into a `LongAccumulator` falls back to the boxed
`noAccumulatorFactoryInfo` path and the mistyped instances are never
selectable. Unchanged from main. NEEDS-HUMAN at lines ~763/771.

## `library/src/scala/collection/convert/impl/BitSetStepper.scala:~92-110` - `semiclone` leaves stale `found`

`semiclone(half)` has three state-transfer paths; the two paths that move
`i0` to `half` clear `found`, but the third (underlying non-null, `half`
within the cached words) copies `found` to the new stepper and never clears
this stepper's `found` before `InOrderStepperBase.trySplit` sets `i0 = half`.
`found == true` means "bit `i0` is set", so a stale value makes
`hasStep`/`nextStep` report index `half` as an element without checking that
bit. Reachable via `hasStep()` then `trySplit()` with no intervening
`nextStep()` (direct Stepper API use; the Java Spliterator adapter always
pairs them). Same code ships in Scala 2.13. NEEDS-HUMAN at the site.

## `library/src/scala/collection/immutable/Map.scala:~749, ~972, ~1214` - `Map2/3/4Iterator.drop` does not clamp negative `n`

Each is `override def drop(n: Int) = { i += n; this }` with no `n max 0`
clamp, so `drop(-1)` rewinds the iterator and replays already-returned
elements, and repeated huge drops can overflow `i`. The base
`Iterator.drop`/`sliceIterator` contract treats negative `n` as 0. Same code
in scala/scala. NEEDS-HUMAN at the three sites.

## `library/src/scala/collection/immutable/HashMap.scala:~531` - dead branch in `merged`

`else if (that.size == 0)` follows `else if (that.isEmpty) this`, so it is
unreachable; its body reads a payload from `rootNode` and calls
`rootNode.containsKey` on the key just read from the same node (always
true), suggesting a mistargeted copy of the preceding `size == 1` branch
(presumably meant `that.size == 1`). Behaviour is unaffected (dead code);
no in-file note because `merged` carries pre-existing documentation.

## `library/src/scala/collection/concurrent/TrieMap.scala:72` - `GCAS_READ` null question

The result type admits `null` and `GCAS_Complete` guards `null`, yet the
method dereferences `m.prev` unchecked; whether the main node can actually
be `null` there needs a human answer (also flagged by the automated
reviewer). Doc/contract ambiguity rather than a demonstrated bug.
NEEDS-HUMAN at the site.

## `library/src/scala/collection/mutable/ArrayBuilder.scala:751` (JVM) - `ofUnit.addAll(xs, offset, length)` ignores clamping

The body is `newSize = size + length; ensureSize(newSize); size = newSize`,
ignoring `xs` and `offset`. The base-class 3-arg `addAll` clamps `offset`
and `length`; every other subclass gets that behaviour. For `ofUnit` a
negative `length` silently shrinks the builder (or throws from `resizeUp`
when `size + length < 0`) and an oversized `length` inflates the count past
the array's element count. NEEDS-HUMAN at the site.

## `library-js/src/scala/collection/mutable/ArrayBuilder.scala:~397ff` - `equals` without `hashCode`

The JS file retains `equals` overrides (removed upstream in 2.13) on
`ofRef`...`ofBoolean` and `ofUnit` comparing `(size == x.size) &&
(elems == x.elems)` - reference equality on the backing arrays - with no
`hashCode` override, violating the `equals`/`hashCode` contract (two
freshly-created empty builders of the same class are equal with different
hash codes). NEEDS-HUMAN at `ofRef.equals`.

## `library/src/scala/collection/SeqView.scala` - `Drop`/`DropRight`/`TakeRight` `apply` unchecked against view bounds

The classes carry `@throws[IndexOutOfBoundsException]` annotations but
delegate directly to the underlying sequence without range-checking against
the view's own length: `Seq(1,2,3).view.dropRight(1).apply(2)` returns the
dropped element instead of throwing, and negative indices on
`Drop`/`TakeRight` can return elements outside the view. Identical to
scala/scala 2.13; documented factually ("bounds are checked only by the
underlying sequence") rather than flagged in-file. Same category:
`View.Updated.isEmpty` throws `IndexOutOfBoundsException` instead of
returning a Boolean when the underlying collection is too short.

## `compiler/src/scala/quoted/runtime/impl/QuotesImpl.scala:1812` - `OmitSelector.name` returns a tree rendering, not a name

`OmitSelectorMethods` defines `def name: String = self.imported.toString`.
`imported` is an `untpd.Ident`, so `toString` is the case-class rendering of
the tree (`Ident(bar)`), not the bare name. Both neighbours use
`imported.name.toString`: `SimpleSelectorMethods.name` at :1763 and
`OmitSelector.unapply` at :1806, which is in the same object. So for
`import foo.{bar => _}`, `unapply` yields `"bar"` while the extension method
`name` yields `"Ident(bar)"` for the same selector.

The abstract declaration in `library/src/scala/quoted/Quotes.scala`
(`OmitSelectorMethods.name`) documents the contract as "the name of the
omitted member, e.g. `bar` in `import foo.{bar => _}`", which is what the
siblings implement; the deviation is in `QuotesImpl`, not in the API doc, so
the week 6 branch documents the contract and leaves it at that.

## `library/src/scala/collection/immutable/Vector.scala:~509` - `Vector.last` throws with the message `"empty.tail"`

`last` on an empty vector throws
`new NoSuchElementException("empty.tail")`; `head` immediately above throws
`new NoSuchElementException("empty.head")`, so `"empty.last"` is what the
pattern calls for. Cosmetic: the exception type, which is what callers can
rely on, is correct, so the week 12 branch documents only the `@throws`.
Same message in scala/scala 2.13.
