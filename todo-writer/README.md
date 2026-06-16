# todo-writer

A small Scala 3 tool for working with Scaladoc in the Scala 3 standard library,
plus a reproducible program that counts the documentation effort described in the
accompanying blog post.

Everything here is written in Scala 3 (Scala 3.3.3).

## What the counts are

The blog post cites two numbers about the scale of the standard library's missing
documentation. This project lets anyone reproduce both from scratch:

1. **Tags added across the submitted pull requests.** How many `@param`,
   `@tparam`, and `@return` tags were added to fill in gaps on declarations that
   already had a Scaladoc comment. The relevant PR numbers are listed in
   `src/main/scala/todowriter/Counter.scala`; their diffs are downloaded from
   GitHub and the additions counted directly.

2. **TODOs for completely undocumented declarations.** How many TODO placeholders
   the tool would insert for declarations that have no Scaladoc comment at all.
   This runs the same checker and fixer the tool uses, in memory, over the Scala 3
   library source. Nothing is written.

## Running the counter

From this `todo-writer` directory:

```bash
sbt "runMain todowriter.Counter"
```

It downloads the PR diffs, scans the library source under `../library/src` and
`../library-js/src`, and prints a breakdown and a grand total, for example:

```
TOTAL        @param  3211   @tparam  2116   @return  1476
Tags added (subtotal): 6803

Declarations with no Scaladoc:   5894
TODO placeholders to insert:    21334

GRAND TOTAL (tags added + undocumented TODOs): 28137
```

Notes:

- The tag counts are exact and immutable: a merged or closed PR's diff never
  changes, so the tag numbers are fully reproducible. (The few PRs that were still
  open when the post was written may gain commits until they merge.)
- The undocumented count reflects the library source as it exists in this branch.
  Check out the same commit to get the same number.
- Set `GITHUB_TOKEN` in your environment to raise GitHub's rate limit if you run
  the counter repeatedly. It is not needed for a single run.

## The pull requests counted

These are the 21 pull requests whose `@param`/`@tparam`/`@return` additions make
up the tag count (subtotal 6,803). The program prints the per-PR breakdown when
you run it; the authoritative list lives in `Counter.scala`.

First pass, parts 1-12 of the library (all merged):

- [#25371](https://github.com/scala/scala3/pull/25371) Sys, concurrent, runtime (1)
- [#25372](https://github.com/scala/scala3/pull/25372) Collection mutable (2)
- [#25373](https://github.com/scala/scala3/pull/25373) Collection immutable (3)
- [#25374](https://github.com/scala/scala3/pull/25374) Collection core (4)
- [#25375](https://github.com/scala/scala3/pull/25375) Numeric types (5)
- [#25376](https://github.com/scala/scala3/pull/25376) Array, IArray, Predef, Option (6)
- [#25377](https://github.com/scala/scala3/pull/25377) Math (7)
- [#25378](https://github.com/scala/scala3/pull/25378) Quoted, compiletime (8)
- [#25379](https://github.com/scala/scala3/pull/25379) Jdk (9)
- [#25380](https://github.com/scala/scala3/pull/25380) Util, io, ref (10)
- [#25381](https://github.com/scala/scala3/pull/25381) Annotation, reflect, misc (11)
- [#25996](https://github.com/scala/scala3/pull/25996) Function, Tuple, Product (12) [V2]

Second pass, tags missed the first time (merged):

- [#26119](https://github.com/scala/scala3/pull/26119) Sys, concurrent, runtime (1)
- [#26122](https://github.com/scala/scala3/pull/26122) Collection core (4)
- [#26123](https://github.com/scala/scala3/pull/26123) Math (5)

Second pass (open at the time of writing):

- [#26120](https://github.com/scala/scala3/pull/26120) Collection mutable (2)
- [#26121](https://github.com/scala/scala3/pull/26121) Collection immutable (3)
- [#26124](https://github.com/scala/scala3/pull/26124) Quoted, compiletime (6)
- [#26125](https://github.com/scala/scala3/pull/26125) Root files (9)
- [#26126](https://github.com/scala/scala3/pull/26126) Jdk (7)
- [#26127](https://github.com/scala/scala3/pull/26127) Util, io, ref (8)

### Deliberately excluded

For completeness, these authored PRs are *not* counted, with the reason:

| PR | Reason |
|----|--------|
| [#24754](https://github.com/scala/scala3/pull/24754) | Style-only doc changes; no new tags |
| [#25065](https://github.com/scala/scala3/pull/25065) | Closed, unmerged; Wikidoc-to-Markdown style migration |
| [#25113](https://github.com/scala/scala3/pull/25113) | Merged, but style/migration; reformats tags, net ~0 new |
| [#25268](https://github.com/scala/scala3/pull/25268) | Closed, unmerged; the original PR, split into #25371-25382 (counting it would double-count) |
| [#25382](https://github.com/scala/scala3/pull/25382) | Closed, unmerged; replaced by #25996 (V2) |
| [#26224](https://github.com/scala/scala3/pull/26224) | Adds `& caps.Pure` bounds; contains no doc tags |

The full set of authored PRs is at
<https://github.com/scala/scala3/pulls?q=is%3Apr+author%3Abvenners> (21 counted +
6 excluded = 27), so the two lists above should account for every one.

## The Scaladoc tool itself

The counter reuses the tool's checker and fixer. The tool can also be run directly
to find and fill missing Scaladoc tags:

```bash
# Report what is missing, write nothing:
sbt "run ../library/src --dry"

# Insert TODO placeholders for declarations that have NO Scaladoc at all:
sbt "run ../library/src --only-undocumented"
```

Run `sbt "run --help"` for the full list of options. Scan the `src` directories
(not `target`) so generated Scala 2 library sources are not included.

## Tests

```bash
sbt test
```
