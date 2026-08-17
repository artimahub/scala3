# Adversarial review: PR #75, `scaladoc-missing-docs-util-concurrent`

**Reviewed:** branch `scaladoc-missing-docs-util-concurrent` @ `613c1c9b32` against `upstream/main` @ `3adfcbd32a`
**Scope:** 36 files, 1,754 insertions, 0 deletions, 442 doc comments added or modified
**Date:** 2026-08-16

## Verdict

The mechanical quality is high and the diff is structurally safe. The factual
quality is uneven, and the unevenness is not random: it concentrates in
`Future.never`, where the generated prose describes what a `Future` combinator
normally does rather than what these overrides actually do. Three statements are
outright false, and fifteen `@return` tags describe a value the method cannot
produce.

Everything below was verified against the implementation, not inferred from the
prose.

**Blockers** (wrong, would mislead a reader): 3
**Should fix before merge:** 15 contradictory `@return` tags, 8 missing caveats
**Nits:** 26 redundant `@return`, 2 self-referential openings, 59 low-value one-liners

A note on my own method, since it bears on how much weight to give the clean
results: my first pass scanned whole files rather than the diff, and reported
five `@param` errors and two `@tparam` errors that turned out to be pre-existing
upstream documentation, not this PR's work. Every count below is scoped to lines
this PR actually adds. Treat any figure I report as trustworthy only to the
extent that scoping held.

## What is clean

These were checked across all 442 added comments and came back with nothing:

| Check | Result |
|---|---|
| `@param` naming a parameter that does not exist | 0 |
| `@tparam` naming a type parameter that does not exist | 0 |
| Unknown or misspelled Scaladoc tags | 0 |
| Annotation syntax smuggled into a doc tag | 0 |
| Unbalanced backticks | 0 |
| Empty `@param` / `@tparam` / `@return` tags | 0 |
| `TODO FILL IN` markers remaining | 0 |
| Doc comments placed below their annotation (parser drops these) | 0 |
| Non-comment lines changed | 0 in all 36 files |

The last row is the important one for reviewer confidence: the diff is
provably comment-only. Every file's non-comment, non-blank lines are
byte-identical to `upstream/main`.

`Duration.Infinite` deserves specific praise. Its arithmetic docs enumerate the
real match cases and get them right:

```scala
/**  - Adding two different infinite durations (Inf + MinusInf) results in Undefined
 *   - Adding an infinite duration with Undefined results in Undefined
 *   - Adding an infinite duration with a finite duration results in the same infinite duration
 */
def +(other: Duration): Duration = other match {
  case x if x eq Undefined      => Undefined
  case x: Infinite if x ne this => Undefined
  case _                        => this
}
```

That is an accurate, useful description of non-obvious behaviour. `*` and `/`
are equally faithful, including the `NaN` and zero cases.

## Blockers

### B1. `Future.never.result` documents a success path that cannot exist

`library/src/scala/concurrent/Future.scala:654`

```scala
/** Returns the result of this future if it's completed within the specified timeout.
 *
 *  @param atMost the maximum duration to wait
 *  @param permit the permission to block
 */
@throws[TimeoutException]
@throws[InterruptedException]
override final def result(atMost: Duration)(implicit permit: CanAwait): Nothing = {
  ready(atMost)
  timeoutError(atMost)
}
```

The return type is `Nothing`. The method cannot return a result; it always
throws. There is no "if it's completed" case, because `never` is by definition
never completed. The declared type and the two `@throws` annotations, both
already present upstream, contradict the sentence directly above them.

Suggested: *"Always throws. This future is never completed, so this method waits
for `atMost` and then throws a `TimeoutException`, or throws
`InterruptedException` if the thread is interrupted first."*

### B2. `Future.never.ready` claims it can complete, and documents a return it never makes

`library/src/scala/concurrent/Future.scala:620`

```scala
/** Blocks until this future is completed or the specified timeout is reached.
 *
 *  @return this future instance
 */
override final def ready(atMost: Duration)(implicit permit: CanAwait): this.type = {
  ...
  timeoutError(atMost)   // every path reaches this, or throws InterruptedException
}
```

Two errors. "Until this future is completed" describes an event that cannot
occur. `@return this future instance` is false: every path through the method
ends in `timeoutError`, which throws, or in `throw new InterruptedException`.
The method never returns normally despite its `this.type` signature.

### B3. `MissingParentBlockContext.blockOn` documents a returned result that is always discarded

`library/src/scala/concurrent/BatchingExecutor.scala:39`

```scala
/** Throws an `IllegalStateException` indicating a bug when `parentBlockContext` is null.
 *
 *  @return the result of the `thunk`
 */
override def blockOn[T](thunk: => T)(implicit permission: CanAwait): T =
  try thunk finally throw new IllegalStateException("BUG in BatchingExecutor.Batch: parentBlockContext is null")
```

`@return the result of the thunk` is false. The `finally throw` discards the
thunk's value unconditionally, so the method never returns. The description is
also misleading in a second way: there is no null test here. Reaching this
`blockOn` at all is the bug, so it throws every time it is called, not "when
`parentBlockContext` is null".

## Should fix before merge

### S1. Fifteen `@return` tags in `Future.never` contradict the method body

Every combinator on `never` is `= this`, returning the same never-completed
future. Fifteen of them carry an `@return` describing a new or eventually
completed future:

```
transform:699   transform:708   transformWith:718   map:728    flatMap:738
flatten:748     filter:755      collect:762         recover:770 recoverWith:780
zip:790         zipWith:799     fallbackTo:809      mapTo:818  andThen:825
```

The clearest are `map` and `flatMap`, where the comment contradicts *itself*:

```scala
/** Creates a new future by applying a function to the successful result of this future.
 *
 *  Since this future is never completed, this method returns the future itself.   <- true
 *
 *  @return a future which will be completed with the result of the application of the function
 */                                                                                 ^ false
override final def map[S](f: Nothing => S)(implicit executor: ExecutionContext): Future[S] = this
```

The description states the truth and the tag denies it. A reader scanning the
generated API page sees the tag.

These `@return` tags should either be dropped or rewritten to say the future
itself is returned and is never completed.

### S2. Eight `never` members lack the caveat their siblings have

Fifteen members carry "Since this future is never completed, ...". These eight do
not, and read as ordinary `Future` documentation:

```
ready:620   result:654   transform:699   flatten:748
filter:755  collect:762  zipWith:799     mapTo:818
```

`ready` and `result` are B2 and B1 above. The other six are merely incomplete
rather than false, but the inconsistency is conspicuous: two `transform`
overloads sit adjacent, one with the caveat and one without.

## Nits

### N1. 26 `@return` tags restate the description verbatim

The project's own rule (`doc-writer-prompt-direct.txt`, the `@return` section)
says to drop `@return` when the description already begins with "Returns" and
states the whole return value. 26 added tags violate it. `Duration.scala` holds
most of them:

```scala
/** Returns the length of this duration measured in whole nanoseconds, rounding towards zero.
 *  @return the length of this duration in nanoseconds
 */
```

The tag contributes nothing. Same shape at `Duration.scala:522, 529, 536, 543,
550, 557, 564` and at `293` (`Returns false since this is an infinite duration.`
/ `@return false`).

A further 46 `@return` tags in the diff do add real information (edge cases,
sentinels, units) and should stay. The rule is working most of the time; this is
the tail.

### N2. Two self-referential openings

The style guide prefers describing the thing directly over "This class ...".

- `Future.scala:1114` — "This trait is deprecated and superseded by
  `scala.concurrent.Batchable`." This also merely restates the `@deprecated`
  annotation on the next line, which scaladoc already renders.
- `concurrent/impl/Promise.scala:694` — "This method is called by the
  ExecutionContext when the transformation is ready to be executed."

### N3. 59 one-line docs that spell out the identifier

Roughly one in seven added comments restates the declaration's name in words
without adding information, e.g. a `runLimit` documented as "The maximum number
of `Runnable` tasks processed in a single batch execution." These are harmless
and arguably better than nothing, but they are the least valuable part of the
diff and will attract reviewer comment on a stdlib PR.

## Why the errors cluster where they do

Worth stating plainly for the PR discussion, because it predicts where a
reviewer's attention is best spent.

33 of these 36 files were reviewed for style only. The accuracy reviewer was
pointed at `mistral-medium-2508`, an orphaned model snapshot that returned an
empty approval on every file it saw — 42 consecutive times — so no factual
review took place on them. Only `Future.scala`, `concurrent/duration/package.scala`
and `Regex.scala` had a working accuracy reviewer.

That does not match the error distribution neatly: two of the three blockers are
in `Future.scala`, which *did* get accuracy review. What it does explain is why
the errors survived at all — the review that ran on `Future.scala` happened
before the final content was settled, and the refine step did not act on the
findings it produced. On the `Regex.scala` `groupNames` pair, both reviewers
correctly flagged a duplicated `@deprecated` tag and the text shipped unchanged
until it was fixed by hand.

So the pipeline's reviewers are not the safety net they appear to be, on this
diff. The three blockers above are the kind of thing a careful human reader
catches in minutes and an approving reviewer never will.

## Recommended action

1. Fix B1, B2, B3. They are wrong, not merely awkward, and each is a few lines.
2. Sweep `Future.never` for S1 and S2 as one edit; the object is 222 lines and
   the fix is uniform.
3. Take or leave the nits. N1 is a real rule violation and cheap to fix; N3 is a
   judgement call about whether a thin doc beats no doc.

Nothing here threatens the code: the diff is comment-only and verified so.
