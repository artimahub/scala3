# Handoff: finishing the Scaladoc rollout (for the next model)

You are finishing a documentation project that is ~93% complete. Read these
three files before writing anything:

1. `todo-writer/docs/fable-brief.md` - the original brief: why the work
   exists, the rules, the official style guide. Everything in it still binds
   you, EXCEPT its inventory of what is done (superseded by this file).
2. `todo-writer/docs/human-review-comments.md` - every review comment a Scala
   maintainer left on this kind of documentation. The defect patterns there
   are the ones that actually happen.
3. `todo-writer/docs/writer-rules.md` - the condensed working rules used for
   all completed weeks. Follow them exactly; they encode the brief plus
   everything learned since. (Its "report back" section addressed subagents;
   for you the equivalent is: record suspected bugs in
   `todo-writer/docs/suspected-bugs.md` and notational observations in
   `todo-writer/docs/consistency-notes.md`, both of which already have many
   entries whose format you should match.)

## State of the branch `fable-scaladoc-weeks-3-11`

Thirteen finished commits sit on top of the tooling commit. Weeks 3-10, week
11 part 1 (the five heavy immutable files), a partial week 11 part 2, two
completion commits (LongMap, TreeSeqMap), and one commit recording suspected
bugs and consistency notes. Every commit was verified comment-only against
`main` with zero `TODO FILL IN` markers in its files. Do not rewrite any of
these commits, and do not revise their documentation: review happens
downstream.

A safety branch `marker-backup` snapshots the working tree; leave it alone.

## Job 1: the remaining week 11 files

Exactly 16 files in `library/src/scala/collection/immutable/` still carry
`TODO FILL IN` markers, 1,027 marker lines total, all uncommitted working-tree
changes (the markers are comment lines sitting in otherwise-unmodified files):

List, Set, Stream, LazyList, LazyListIterable, BitSet, WrappedString,
SortedMap, SortedSet, ListMap, ListSet, Queue, Seq, Iterable,
StrictOptimizedSeqOps, package.

Fill every marker, then make ONE commit containing exactly these 16 files,
message style: `Week 11 (part 2b): Scaladoc for the remaining
collection.immutable files`. It joins the two existing part-2 commits in one
pull request, so consistency with the already-committed immutable files
matters: read the completed docs in `immutable/TreeMap.scala`,
`immutable/Map.scala`, `immutable/HashMap.scala` and `mutable/BitSet.scala`
first and reuse their contract phrasing where members correspond.

File-specific cautions learned on this codebase:

- `Stream.scala` (deprecated) and the two LazyList files: laziness precision
  is the whole point - when the head/tail thunk runs, memoization, what
  `force`/`lazyAppendedAll` actually defer. The platform-split base files
  (`LazyListBase.scala`, `LazyListIterableBase.scala`) are already documented;
  match their terminology. Do not restate `@deprecated` annotations.
- `Stream.scala` has markers sitting BELOW `@deprecated`/`@SerialVersionUID`
  annotations (e.g. line ~29). A doc comment below an annotation is dropped by
  the parser: always place the finished doc ABOVE the annotation, moving only
  comment lines.
- `Set.scala` has the Set1-Set4 small-arity family; mirror the completed
  Map1-Map4 template in `immutable/Map.scala` (and note its known
  contract deviation pattern: the small-map iterators' `drop` does not clamp
  negative n - if Set's iterators share that shape, document neutrally and
  flag, as was done for Map).
- `ListMap`/`ListSet` are O(n) list-backed structures; the MUTABLE ListMap's
  pre-existing class doc turned out to be false about ordering - verify what
  the immutable ones actually preserve before claiming order.
- `WrappedString`: an immutable wrapper over String; say what is O(1) via
  the underlying String and what falls back to generic Seq behaviour.
- `BitSet`/`SortedMap`/`SortedSet`/`Seq`/`Iterable`/`StrictOptimizedSeqOps`/
  `package`: mostly trait plumbing and factories; the corresponding
  `collection/` and `mutable/` files are all documented - stay consistent.

Verification before the commit (all three must pass, per file):

```bash
f=library/src/scala/collection/immutable/List.scala
diff <(git show main:$f | grep -vE '^\s*(\*|/\*\*|\*/)' | grep -v '^\s*$') \
     <(grep -vE '^\s*(\*|/\*\*|\*/)' $f | grep -v '^\s*$')   # must be empty
grep -ac "TODO FILL IN" $f                                    # must be 0
grep -nE '^\s*\*.*@deprecated\(' $f                           # must be empty
```

Also check: no `@throws` whose first token is not an exception class name, no
two adjacent doc comments, no em-dashes (U+2014), and never any control
character (a previous run wrote a literal NUL byte and made a file binary -
use `grep -a` when counting markers, and write char literals as escapes).

## Job 2: weeks 1 and 2 (separate, conservative)

Per the original brief's final section: two open, undrafted PRs a maintainer
may be reading right now.

| week | PR | branch |
|---|---|---|
| 1 | #26429 | `scaladoc-missing-docs-io-ref-numeric` |
| 2 | #26657 | `scaladoc-missing-docs` |

For each, separately: branch off that PR branch, read the documentation the
PR added (its diff against its merge-base with `main`), improve ONLY what is
false, misleading, or a notational break of the kind the brief describes, and
make one commit on that branch - or make no commit and say so if nothing
clears that bar (a legitimate outcome). Every changed line is one a
maintainer must re-review; prose you would merely have phrased differently
does not qualify. Do these AFTER job 1, and do not mix their commits with the
main branch.

## How the work was done (so you can match it)

Documentation was written by reading every implementation before describing
it, checking overridden members and siblings, and never guessing: questions
the code cannot answer get `@note NEEDS-HUMAN:` at the site; suspected bugs
get an entry in `suspected-bugs.md` and are never documented as intended
behaviour. Families get one template varied only by what differs. `@return`
is dropped when the description starts with "Returns" and says it all.
Everything else is in `writer-rules.md`.

Work single-threaded unless the user says otherwise; they are managing a
usage budget and asked for one-thing-at-a-time with a commit at each safe
point.
