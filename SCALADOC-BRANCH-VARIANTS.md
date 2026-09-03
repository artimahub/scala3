# Scaladoc PR branch variants - what is what

Written 2026-08-28, at the end of the Scaladoc documentation project; checked
in 2026-09-03 so the decisions behind the 48 branches are recorded alongside
them. See the *Update* notes below for what has changed since.

## The short version

The twelve open documentation PRs were left **completely untouched**. On top of
each, four new local branches were built so that a decision about internal
documentation can be made later without redoing any work. Nothing has been
pushed.

```
<branch>                     the PR branch, untouched
├── -curated                 1 commit: remove low-value internal docs
│   └── -boilerplate         1 commit: put them back      (tree == <branch>)
└── -api-only                1 commit: remove ALL internal docs
    └── -internal            1 commit: put them back      (tree == <branch>)
```

`-curated` + `-boilerplate` recomposes the PR branch exactly. So does
`-api-only` + `-internal`. Verified per branch, not assumed.

## The four suffixes

| suffix | contains |
|---|---|
| `-curated` | The PR branch minus the 1,991 internal doc comments that only restate the signature. Keeps the 319 that carry an invariant, protocol, or cross-platform contract. |
| `-boilerplate` | Just those 1,991, restored on top of `-curated`, so they can go up as their own PR if wanted. |
| `-api-only` | The PR branch minus **all** 2,310 non-user-facing doc comments. Leaves only what the published API pages render. |
| `-internal` | Just those 2,310, restored on top of `-api-only`. |

Two pairs, two policies. `-curated` applies judgement; `-api-only` applies a
mechanical rule.

## Why "non-user-facing"

A declaration is user-facing when a programmer outside the library can reach
it: public, or `protected` in any form. Only the `private` forms - `private`,
`private[x]`, and anything nested inside such a scope - are hidden.

> **Update, 2026-09-03.** The rule used when these branches were built was
> narrower, and wrong: it also treated `protected[x]` and `protected[this]` as
> non-user-facing, following scaladoc's `isHiddenByVisibility`
> (`scaladoc/src/dotty/tools/scaladoc/tasty/SymOps.scala:126`). The qualifier
> *widens* `protected` rather than narrowing it - per SLS 5.2.2 such members
> are "**also** accessible" from within `C`, keeping ordinary protected access
> for every subclass - so a subclass author can still reach them. Scaladoc
> hides them anyway, based on whether a companion object exists; that is a
> scaladoc bug, written up in
> `todo-writer/docs/scaladoc-protected-visibility-bug.md`.
>
> Effect on this batch: of 205 doc comments dropped from `protected` members,
> 156 sat inside a `private` scope and were correctly dropped. Of the
> remaining 49, all but 8 are in `final`, `sealed`, or `object` types that
> cannot be extended from outside. Those **8 were restored** by one commit on
> each of `-util-concurrent`, `-collection-mutable` and `-collection-core`.
> `split-internal-docs.py` has since been corrected, so a rerun would not drop
> them again. The counts throughout this note predate that correction.

**2,310 of 8,147 documented declarations (28.4%) are hidden this way.** About
2,000 of them carry no modifier of their own and are hidden only because an
enclosing object or class is private - which is how they came to be written.

This all started from a PR review comment on
`library-js/.../LazyListBase.scala` asking whether internal implementation
details belong in public documentation. Worth remembering: that specific
example turned out to be a *keeper*. The reviewer quoted the summary line
only; the sentence below it explains that Scala.js is single-threaded and so
does not need the JVM's atomicity. The general point still stood for the other
1,991.

## The twelve branches

| week | branch | `-curated` cuts | `-api-only` cuts | keepers |
|---|---|---:|---:|---:|
| ? | `scaladoc-missing-docs-annotation-reflect-root` | 81 | 82 | 1 |
| ? | `scaladoc-missing-docs-misc-dirs-root-misc` | 79 | 80 | 1 |
| 3 | `scaladoc-missing-docs-core-array-function-tuple-sys` | 148 | 153 | 5 |
| 4 | `scaladoc-missing-docs-util-concurrent` | 140 | 147 | 7 |
| 5 | `scaladoc-missing-docs-math-coll-generic` | 87 | 149 | 62 |
| 6 | `scaladoc-missing-docs-quoted-jdk` | 26 | 26 | 0 |
| 7 | `scaladoc-missing-docs-collection-convert-js` | 461 | 514 | 53 |
| 8 | `scaladoc-missing-docs-runtime` | 16 | 17 | 1 |
| 9 | `scaladoc-missing-docs-collection-mutable` | 158 | 214 | 56 |
| 10 | `scaladoc-missing-docs-collection-core` | 173 | 179 | 6 |
| 11 | `scaladoc-missing-docs-collection-immutable-vector-hashmap-arrayseq` | 410 | 425 | 15 |
| 12 | `scaladoc-missing-docs-collection-immutable-list-lazylist-sorted` | 212 | 324 | 112 |
| | **total** | **1,991** | **2,310** | **319** |

Counts are declarations. In comment lines: `-curated` removes 8,720 and
`-api-only` 10,912, out of 42,431 added across the batch.

`scaladoc-missing-docs-io-ref-numeric` (week 1, an older separately opened PR)
is **not** part of this batch and has no variants.

## What the keepers are

The 319 fall under three overlapping rules:

1. Every hidden `class`/`trait`/`object` (60). A type with no documentation at
   all is opaque, and there are only sixty.
2. Members whose docs state an invariant, protocol, cross-platform contract, or
   non-obvious constraint (94).
3. Everything in eleven protocol files whose rules live nowhere else (236):
   `TrieMap` (GCAS), both `RedBlackTree`s, `ChampCommon`, the four
   `LazyList*Base` files (JVM and JS), the two stepper base classes,
   `MutationTracker`.

What that deliberately excludes: `semiclone` appears 45 times across the
stepper implementations. The two base-class declarations state the split
contract; the other forty restate the signature once per element type. Base
kept, repeats dropped.

## Gotchas to remember

- **`quoted-jdk` has zero keepers**, so its `-curated` and `-api-only` are
  content-identical. Not a bug. If demonstrating the difference between the two
  policies, use week 12, 9, or 5 instead.
- **Six files across five branches are "held back"** and keep all their
  documentation on every variant. These are files where a week also rewrote
  pre-existing comment text; reverting an added block there would drop the text
  it replaced. Listed at the end of `internal-docs-inventory.md`.
- The classifier is a regex plus an indentation-based scope walker, not a
  compiler front end. It is not relied on for correctness: the tooling verifies
  that the two halves recombine to exactly the original tree, that each thinned
  branch is still comment-only against its merge base, and that no
  `TODO FILL IN` marker reappears.

## Pushing

The 48 variant branches were new refs, so pushing them needed no force and
could not affect the open PRs:

```bash
for B in $(git branch --format='%(refname:short)' | grep -E -- '-(curated|boilerplate|api-only|internal)$'); do
  git push origin "$B"
done
```

> **Update, 2026-09-03.** All 48 are now on `origin`. The decision was then
> taken to adopt the `-api-only` policy for the open PRs. Each of the eleven PR
> branches in this batch was fast-forwarded onto its `-api-only` tip locally -
> the tip is exactly the PR branch plus one commit, so this is a fast-forward,
> not a rewrite - and the state each PR was filed with is preserved on a
> `<branch>-as-filed` backup ref. Three of them carry a second commit restoring
> the eight protected docs described above. Nothing had been pushed to the PR
> branches as of this note; `git log --oneline -2` on any of them shows what is
> pending.
>
> `scaladoc-missing-docs-annotation-reflect-root` has variants but no open PR,
> so it is not among the eleven.

## Supporting material (committed, on `fable-scaladoc-weeks-3-11`)

| file | what |
|---|---|
| `todo-writer/docs/internal-docs-inventory.md` | full inventory: totals, per-branch, wholly-internal files, held-back files |
| `todo-writer/docs/internal-docs-keepers.md` | the 319 keepers, per file, with the reasoning |
| `todo-writer/reviews/internal-docs.json` | every documented declaration with its visibility and doc line numbers |
| `todo-writer/reviews/internal-docs-keepers.json` | the keep-list the tooling consumes |
| `todo-writer/scripts/split-internal-docs.py` | `report`, `split`, `variants` subcommands; dry-run by default |
| `todo-writer/docs/suspected-bugs.md` | 21 suspected bugs found while documenting |
| `todo-writer/docs/consistency-notes.md` | pre-existing documentation problems, not touched |
| `todo-writer/reviews/branches/` | the independent per-branch reviews that drove adjudication |

To rebuild the variants from scratch:

```bash
python3 todo-writer/scripts/split-internal-docs.py report      # refresh the data
python3 todo-writer/scripts/split-internal-docs.py variants    # dry run
python3 todo-writer/scripts/split-internal-docs.py variants --apply
```
