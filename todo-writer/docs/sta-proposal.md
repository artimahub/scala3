# STA Proposal: Standard Library Quality-of-Life Improvements

**Author:** Bill Venners
**Date:** April 2026

## Overview

This proposal targets **10 small, high-impact additions** to the Scala standard library. Each item is:

- A single method or small set of related methods
- Easy to understand and explain to users
- Straightforward to implement with minimal design debate
- Binary compatible (additions only, no removals or signature changes)
- Backed by community demand (forum posts, GitHub issues, thumbs-ups)

Together they tell a clear story: **practical, everyday improvements that Scala developers will immediately appreciate.**

---

## Proposed Additions

### 1. `frequencies` on collections

**Source:** [Contributors thread #7](https://contributors.scala-lang.org/t/standard-library-now-open-for-improvements-and-suggestions/7337/7)

Count occurrences of each element in a collection. Currently requires the common but verbose `groupBy(identity).view.mapValues(_.size)` pattern.

```scala
List("a", "b", "a", "c", "b", "a").frequencies
// Map("a" -> 3, "b" -> 2, "c" -> 1)
```

**Why:** Extremely common operation. Python has `Counter`, Kotlin has `groupingBy + eachCount`. One of the most-requested convenience methods.

---

### 2. `zipStrict` on collections

**Source:** [Contributors thread #24](https://contributors.scala-lang.org/t/standard-library-now-open-for-improvements-and-suggestions/7337/24)

Zip two collections, returning `None` if they differ in length instead of silently truncating.

```scala
List(1, 2, 3).zipStrict(List("a", "b", "c"))
// Some(List((1, "a"), (2, "b"), (3, "c")))

List(1, 2).zipStrict(List("a", "b", "c"))
// None
```

**Why:** Silent truncation from `zip` is a common source of bugs. This provides a safe alternative without throwing exceptions.

---

### 3. `unionWith` on Map

**Source:** [Contributors thread #24](https://contributors.scala-lang.org/t/standard-library-now-open-for-improvements-and-suggestions/7337/24)

Merge two maps, using a provided function to resolve key conflicts.

```scala
val m1 = Map("a" -> 1, "b" -> 2)
val m2 = Map("b" -> 3, "c" -> 4)
m1.unionWith(m2)(_ + _)
// Map("a" -> 1, "b" -> 5, "c" -> 4)
```

**Why:** Prior art already exists in `LongMap` and `IntMap`. This generalizes it to all Maps. Very common need when combining maps.

---

### 4. `mapAccumulate` on collections

**Sources:** [Contributors thread #24](https://contributors.scala-lang.org/t/standard-library-now-open-for-improvements-and-suggestions/7337/24), [#46](https://contributors.scala-lang.org/t/standard-library-now-open-for-improvements-and-suggestions/7337/46), [scala-library-next #122](https://github.com/scala/scala-library-next/issues/122)

Map over a collection while threading an accumulator through, returning both the final accumulator and the mapped collection.

```scala
List(1, 2, 3).mapAccumulate(0) { (sum, x) =>
  (sum + x, s"running total: ${sum + x}")
}
// (6, List("running total: 1", "running total: 3", "running total: 6"))
```

**Why:** Requested independently by multiple people (forum #24, #46, and library-next #122 as `mapWithState`). Common pattern that currently requires awkward `foldLeft` with manual collection building.

---

### 5. `groupByOrdered` on collections

**Sources:** [Contributors thread #6](https://contributors.scala-lang.org/t/standard-library-now-open-for-improvements-and-suggestions/7337/6), [scala-collection-contrib #252](https://github.com/scala/scala-collection-contrib/issues/252)

Like `groupBy`, but preserving encounter order of keys via `LinkedHashMap` (or similar).

```scala
List("banana", "apple", "blueberry", "avocado").groupByOrdered(_.head)
// LinkedHashMap('b' -> List("banana", "blueberry"), 'a' -> List("apple", "avocado"))
```

**Why:** `groupBy` makes no ordering guarantees, which frequently surprises users. This fills a genuine gap. Already identified as "missing functionality" in scala-collection-contrib.

---

### 6. `groupFlatMap` on collections

**Sources:** [scala-library-next #135](https://github.com/scala/scala-library-next/issues/135), [PR #136](https://github.com/scala/scala-library-next/pull/136)

The missing method between `groupBy` and `groupMap` — groups and then flatMaps the values.

**Why:** Logically completes the `groupBy` / `groupMap` family. A PR already exists (scala-library-next #136), reducing implementation effort.

---

### 7. `clamp` in scala.math (and on numeric types)

**Source:** [scala-library-next #55](https://github.com/scala/scala-library-next/issues/55)

Constrain a value to a range.

```scala
import scala.math.clamp
clamp(15, 0, 10)  // 10
clamp(-5, 0, 10)  // 0
clamp(7, 0, 10)   // 7
```

**Why:** Present in C++ (`std::clamp`), Rust, Kotlin, Java 21+, and many other languages. Universally understood. Trivial to implement.

---

### 8. `tailOption` and `initOption` on collections

**Source:** [scala-library-next #165](https://github.com/scala/scala-library-next/issues/165)

Safe versions of `tail` and `init` that return `None` on empty collections instead of throwing.

```scala
List(1, 2, 3).tailOption  // Some(List(2, 3))
List.empty[Int].tailOption // None
```

**Why:** Consistent with the existing `headOption` and `lastOption`. Fills an obvious gap in the API. Straightforward to implement and explain.

---

### 9. Bitwise shift operators (`<<`, `>>`) on BitSet

**Source:** [scala-library-next #47](https://github.com/scala/scala-library-next/issues/47)

Shift all bit positions left or right.

```scala
BitSet(1, 3, 5) << 2  // BitSet(3, 5, 7)
BitSet(2, 4, 6) >> 1  // BitSet(1, 3, 5)
```

**Why:** Completes the set of bitwise operations on BitSet. Useful for dynamic programming (subset sum / knapsack). Straightforward to implement.

---

### 10. `lazyZipAll` on Iterable

**Source:** [scala-library-next #62](https://github.com/scala/scala-library-next/issues/62)

A lazy version of `zipAll`, analogous to how `lazyZip` relates to `zip`.

**Why:** Completes an obvious gap for consistency. `lazyZip` exists but `lazyZipAll` does not, mirroring the `zip` / `zipAll` pair. Small, well-defined scope.

---

## Summary

| # | Method | Where | Complexity |
|---|--------|-------|------------|
| 1 | `frequencies` | IterableOnce | Small |
| 2 | `zipStrict` | IterableOnce | Small |
| 3 | `unionWith` | Map | Small |
| 4 | `mapAccumulate` | IterableOnce | Medium |
| 5 | `groupByOrdered` | IterableOnce | Medium |
| 6 | `groupFlatMap` | IterableOnce | Small (PR exists) |
| 7 | `clamp` | scala.math | Small |
| 8 | `tailOption` / `initOption` | IterableOnce | Small |
| 9 | `<<` / `>>` | BitSet | Small |
| 10 | `lazyZipAll` | Iterable | Small |

**Estimated effort:** Each item is individually small (days, not weeks). The bundle is scoped to be achievable within a single STA funding cycle.

**Impact:** These are the kind of additions that show up in "What's new in Scala 3.x" blog posts and get immediate positive community reception — practical, unsurprising, and long-requested.

---

## Tier 2: Also Good, Slightly Larger or With Minor Design Questions

These are positive candidates that may involve a bit more surface area or have a small design question to resolve. Worth considering if Tier 1 items go quickly, or as alternatives.

### T2-1. `frequenciesBy`, `zipWith`, `uncons`, `unconsWith`

**Source:** [Contributors thread #47](https://contributors.scala-lang.org/t/standard-library-now-open-for-improvements-and-suggestions/7337/47)

A bundle of small utility methods. `frequenciesBy` generalizes `frequencies` with a key function. `zipWith` combines zip + map in one pass. `uncons` destructures a collection into head and tail.

```scala
List("apple", "avocado", "banana").frequenciesBy(_.head)
// Map('a' -> 2, 'b' -> 1)

List(1, 2, 3).zipWith(List(10, 20, 30))(_ + _)
// List(11, 22, 33)

List(1, 2, 3).uncons
// Some((1, List(2, 3)))
```

**Why:** Each is small and useful. Natural companions to Tier 1 items (`frequenciesBy` pairs with `frequencies`).

---

### T2-2. Stream/Iterator `gather`-like API

**Source:** [Contributors thread #32](https://contributors.scala-lang.org/t/standard-library-now-open-for-improvements-and-suggestions/7337/32)

A stateful transformation combinator inspired by Java's `Stream.gather` — more powerful than `map` or `flatMap`, allowing intermediate state.

**Why:** Addresses a real gap in collection transformations. May need some design thought on the exact signature.

---

### T2-3. `mutable.Map.merge`

**Source:** [scala-library-next #164](https://github.com/scala/scala-library-next/issues/164)

Merge a key-value pair into a mutable Map, similar to Java's `Map.merge`.

```scala
val m = mutable.Map("a" -> 1)
m.merge("a", 2)(_ + _)  // m is now Map("a" -> 3)
m.merge("b", 5)(_ + _)  // m is now Map("a" -> 3, "b" -> 5)
```

**Why:** Familiar to developers coming from Java. Fairly straightforward to design. Minor question around null handling.

---

### T2-4. `Map.getAndRemove` / `extract`

**Source:** [scala-library-next #86](https://github.com/scala/scala-library-next/issues/86)

Remove a key from a map and return both the removed value and the updated map.

```scala
val m = Map("a" -> 1, "b" -> 2)
m.extract("a")  // Some((1, Map("b" -> 2)))
m.extract("z")  // None
```

**Why:** Avoids the awkward `updatedWith` + `var` pattern. Clean API for a common operation.

---

### T2-5. `filterKeysStrict` / `mapValuesStrict` on Map

**Source:** [scala-library-next #176](https://github.com/scala/scala-library-next/issues/176)

Strict (eager) versions of `filterKeys` and `mapValues`, which are currently lazy views.

```scala
val m = Map(1 -> "a", 2 -> "b", 3 -> "c")
m.filterKeysStrict(_ > 1)   // Map(2 -> "b", 3 -> "c")  -- a real Map, not a view
m.mapValuesStrict(_.toUpperCase)  // Map(1 -> "A", 2 -> "B", 3 -> "C")
```

**Why:** The lazy behavior of `filterKeys`/`mapValues` surprises many users. Scaladoc already promises strict versions will be added. Naming is mostly settled.

---

### T2-6. `String.splitToIArray` (or `splitAsList`)

**Source:** [scala-library-next PR #113](https://github.com/scala/scala-library-next/pull/113)

`String.split` returns a mutable `Array`. Provide a variant returning an `IArray` (or `List`).

```scala
"a,b,c".splitToIArray(",")  // IArray("a", "b", "c")
```

**Why:** Thin wrapper around the existing `split`. Returns an immutable type, which is more idiomatic Scala.

---

### T2-7. `groupMapTo` / `groupByTo` on collections

**Source:** [scala-library-next PR #53](https://github.com/scala/scala-library-next/pull/53)

Fluent API allowing you to specify the target collection type for grouped results.

**Why:** A PoC PR already exists. Extends the `groupBy`/`groupMap` family with more flexibility.

---

## Tier 2 Summary

| # | Method | Where | Complexity |
|---|--------|-------|------------|
| T2-1 | `frequenciesBy` / `zipWith` / `uncons` | IterableOnce | Small each |
| T2-2 | `gather`-like API | Iterator / LazyList | Medium |
| T2-3 | `mutable.Map.merge` | mutable.Map | Small |
| T2-4 | `extract` / `getAndRemove` | Map | Small |
| T2-5 | `filterKeysStrict` / `mapValuesStrict` | Map | Small |
| T2-6 | `splitToIArray` | String | Small |
| T2-7 | `groupMapTo` / `groupByTo` | IterableOnce | Medium (PR exists) |
