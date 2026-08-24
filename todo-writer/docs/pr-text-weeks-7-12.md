# PR titles and descriptions, weeks 7 to 12

Title template: `$a, $b, ... ($num): Add Scaladoc comments for undocumented entities`

The first sentence of each description names the packages; everything after it
is the same boilerplate you have been using.

What the project called week 11a and 11b appear here as weeks 11 and 12. The
split of `collection.immutable` is ours, not something a reviewer needs to know,
so neither the titles nor the branch names mention it.

---

## Week 7

**Branch:** `scaladoc-missing-docs-collection-convert-js`
**40 files, 971 new doc comments**

**Title**

    collection, convert, js (7): Add Scaladoc comments for undocumented entities

**Description**

This PR fills in a main doc comment plus @param, @tparam, and @return tags for
`scala.collection.convert` and the Scala.js variants of the standard library
that are completely missing any Scaladoc documentation. Most of it is the
stepper implementations under `collection.convert.impl`; the rest is the
Scala.js counterparts of files whose JVM versions are documented in the other
pull requests in this series, across `scala`, `scala.collection.immutable`,
`scala.collection.mutable`, `scala.runtime`, `scala.reflect`, `scala.util`,
`scala.math`, `scala.concurrent` and `scala.scalajs`. I'm submitting it as a
draft PR so that I can get the CI to run on it, to see if it breaks anything,
and to start getting feedback. We automated the generation of these changes and
have not reviewed all of them yet. We will review them all before making the PR
non-draft. Please let me know whether you think this is going in the right
direction in general, and anything specific that you notice that could be
improved.

---

## Week 8

**Branch:** `scaladoc-missing-docs-runtime`
**119 files, 688 new doc comments**

**Title**

    runtime (8): Add Scaladoc comments for undocumented entities

**Description**

This PR fills in a main doc comment plus @param, @tparam, and @return tags for
`scala.runtime`, `scala.runtime.java8` and `scala.runtime.coverage` APIs that
are completely missing any Scaladoc documentation. Most of the files are the 87
of `scala.runtime.java8` that back Java function interop; the rest is the
value-class runtime support, the array and tuple helpers, and the rich wrapper
classes. I'm submitting it as a draft PR so that I can get the CI to run on it,
to see if it breaks anything, and to start getting feedback. We automated the
generation of these changes and have not reviewed all of them yet. We will
review them all before making the PR non-draft. Please let me know whether you
think this is going in the right direction in general, and anything specific
that you notice that could be improved.

---

## Week 9

**Branch:** `scaladoc-missing-docs-collection-mutable`
**40 files, 1,032 new doc comments**

**Title**

    collection, mutable (9): Add Scaladoc comments for undocumented entities

**Description**

This PR fills in a main doc comment plus @param, @tparam, and @return tags for
`scala.collection.mutable` APIs that are completely missing any Scaladoc
documentation. It covers the buffers and array-backed sequences, the hash and
tree maps and sets, the builders and the specialised maps such as `LongMap`,
`AnyRefMap` and `OpenHashMap`. I'm submitting it as a draft PR so that I can get
the CI to run on it, to see if it breaks anything, and to start getting
feedback. We automated the generation of these changes and have not reviewed all
of them yet. We will review them all before making the PR non-draft. Please let
me know whether you think this is going in the right direction in general, and
anything specific that you notice that could be improved.

---

## Week 10

**Branch:** `scaladoc-missing-docs-collection-core`
**36 files, 989 new doc comments**

**Title**

    collection, core (10): Add Scaladoc comments for undocumented entities

**Description**

This PR fills in a main doc comment plus @param, @tparam, and @return tags for
the root of `scala.collection` that is completely missing any Scaladoc
documentation. It covers the `Iterable`, `Seq`, `Set` and `Map` hierarchies and
their `Ops` traits, the views, the steppers and stepper shapes, and the factory
and build-from machinery. I'm submitting it as a draft PR so that I can get the
CI to run on it, to see if it breaks anything, and to start getting feedback. We
automated the generation of these changes and have not reviewed all of them yet.
We will review them all before making the PR non-draft. Please let me know
whether you think this is going in the right direction in general, and anything
specific that you notice that could be improved.

---

## Week 11

**Branch:** `scaladoc-missing-docs-collection-immutable-vector-hashmap-arrayseq`
**5 files, 775 new doc comments**

**Title**

    collection, immutable, vector, hashmap, arrayseq (11): Add Scaladoc comments for undocumented entities

**Description**

This PR fills in a main doc comment plus @param, @tparam, and @return tags for
the indexed and hashed collections of `scala.collection.immutable` that are
completely missing any Scaladoc documentation: `Vector`, `ArraySeq`, `HashMap`,
`HashSet` and `Map`. It is only five files but they are the largest in the
package, and it includes the CHAMP trie internals that back the hash
collections and the small-arity `Map1` to `Map4` family. I'm submitting it as a
draft PR so that I can get the CI to run on it, to see if it breaks anything,
and to start getting feedback. We automated the generation of these changes and
have not reviewed all of them yet. We will review them all before making the PR
non-draft. Please let me know whether you think this is going in the right
direction in general, and anything specific that you notice that could be
improved.

---

## Week 12

**Branch:** `scaladoc-missing-docs-collection-immutable-list-lazylist-sorted`
**29 files, 985 new doc comments**

**Title**

    collection, immutable, list, lazylist, sorted (12): Add Scaladoc comments for undocumented entities

**Description**

This PR fills in a main doc comment plus @param, @tparam, and @return tags for
the linear, lazy and ordered collections of `scala.collection.immutable` that
are completely missing any Scaladoc documentation: `List`, `Set` with its
small-arity `Set1` to `Set4` family, the lazy sequences `LazyList`,
`LazyListIterable` and the deprecated `Stream`, the red-black tree maps and
sets, the sorted and list-backed maps and sets, `Queue`, `WrappedString`, the
ranges and the integer-keyed maps. I'm submitting it as a draft PR so that I can
get the CI to run on it, to see if it breaks anything, and to start getting
feedback. We automated the generation of these changes and have not reviewed all
of them yet. We will review them all before making the PR non-draft. Please let
me know whether you think this is going in the right direction in general, and
anything specific that you notice that could be improved.
