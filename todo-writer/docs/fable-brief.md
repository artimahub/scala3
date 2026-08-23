# Brief: finish the Scala 3 standard library Scaladoc rollout

You are writing the missing Scaladoc for the Scala 3 standard library, weeks 3
through 11 of an 11-week plan. Weeks 1 and 2 are already in an open pull request
and are handled separately at the end of this brief.

Everything below is drawn from six weeks of doing this with an automated
pipeline. That pipeline is no longer in charge of this work and you are, so its
failure modes are offered as your rules rather than imposed as its process.

Where a rule looks oddly specific, it is because that exact mistake was made and
caught. Some were caught by the human reviewer on PR #26822, who filed 51
comments on one week's work: a `@param` naming the wrong parameter across 20
methods, a tag saying a function was ignored when the body called it, a class
documented as creating something it does not create. Others were caught by our
own reviewers before any human saw them: an `@throws` that renders as an
exception named "the", a method documented as making a deep copy when it copies
a reference, exceptions promised that erasure makes impossible. Treat both kinds
as equally real; the difference is only who noticed first.

## The job

Scala 3's standard library has roughly 8,360 declarations with **no doc comment
at all**. Earlier PRs handled declarations that already had a comment but were
missing `@param`/`@tparam`/`@return` tags. This work is the remainder.

Every declaration needing documentation has been marked for you with a
`TODO FILL IN` placeholder, in one of three shapes:

```scala
/** TODO FILL IN */                       // a whole missing description

/** TODO FILL IN
 *
 *  @param x TODO FILL IN
 *  @return TODO FILL IN
 */                                       // description and tags

/** Existing prose that is fine.
 *  @param x TODO FILL IN
 */                                       // tags only
```

Replace every marker with real documentation. When you are done, `grep -c "TODO
FILL IN"` over the library must return zero for the weeks you have completed.

## What you will find when you start

A single branch, checked out, containing:

- **One commit per already-completed week**, for weeks 3, 4, 5 and 6. These hold
  documentation written by the retired pipeline. Weeks 3 and 4 are complete but
  drew heavy human review; weeks 5 and 6 are better but week 6 is only partial.

  **None of it is sacred.** Treat that documentation as a suggestion from a
  weaker writer, not as a baseline to preserve. Rewrite as much of it as you
  think the work deserves, up to and including every comment in those four
  weeks. You are being asked for the best documentation you can write, not for
  a minimal diff against what is there.
- **`TODO FILL IN` markers already inserted** for every remaining declaration
  across weeks 3 through 11.

So the earlier weeks' work is visible to you as history, and the outstanding
work is visible as markers.

**You are unrestrained in how you work.** Write by hand, run the existing
tooling, write new tooling, or any mixture. Nothing in `todo-writer/` is
required and nothing is off limits. Some of it may be useful:

- `scripts/adversarial-gate.sh <files>` runs one strong-model pass over a
  finished diff whose only job is to prove the documentation wrong. Run it on
  your own output before committing if you want a second opinion; on week 4 it
  found things four rounds of review had missed.
- `scripts/repeated-doc-blocks.py --orig X --new Y` groups doc blocks and tag
  lines you have written more than once, which is how you check a family for a
  mistake replicated across 40 members.
- `scripts/fill-doc-todos-free.sh` is the retired pipeline itself. Its writer,
  two reviewers and adjudicator can be pointed at any provider or local CLI. It
  is not recommended, but it is there and it works.
- `reviews/*.digest.md` holds every review of weeks 4 through 6, including the
  findings that were never applied.
- `docs/house-rules.md` is a short list of conventions distilled from earlier
  feedback. Append to it if you learn something worth passing on.

The constraints that follow are about the OUTPUT, not the method: comment-only
changes, one commit per week, and documentation that is true of the code.

## The partitions, week by week

| Wk | Paths | ~decls | state |
|----|-------|-------:|-------|
| 3 | `scala/{Array,IArray,Option,Predef}`, `Function*`/`Tuple*`/`Product*`, `sys` | 400 | done, PR #26669 open, rewrite freely |
| 4 | `scala/util`, `scala/concurrent` | 480 | done, PR #26822 open, rewrite freely |
| 5 | `scala/math`, `collection/{generic,concurrent}` | 554 | done, 5 files have known defects, rewrite freely |
| 6 | `scala/quoted`, `scala/compiletime`, `scala/jdk` | 899 | **partial**: `quoted/Quotes.scala` (286 decls) untouched, plus 4 small `quoted` files |
| 7 | `collection/convert`, `library-js` | 901 | not started |
| 8 | `scala/runtime` | 687 | not started, 141 small files |
| 9 | `collection/mutable` | 1048 | not started |
| 10 | `collection` top level, excluding subtrees | 1257 | not started |
| 11 | `collection/immutable` | 1739 | not started; **split into two commits**, heavy files (`Vector`, `HashMap`, `ArraySeq`, `Map`, `HashSet`) and the rest |

## How to commit

**One commit per week, in week order.** This matters more than anything else
about your output: each commit is cherry-picked onto its own branch off `main`
and becomes one pull request. A commit spanning two weeks cannot be used.

The exception is **week 11**, `collection/immutable`, at 1739 declarations. Split
it into two commits, which become two pull requests: one for the heavy files
(`Vector`, `HashMap`, `ArraySeq`, `Map`, `HashSet`) and one for the rest. If any
other week runs far larger than expected once you are in it, splitting it the
same way is fine; keep each commit to a single week's paths so the mapping from
commit to PR stays one to one.

```
Week 7: Scaladoc for collection/convert and library-js
```

For weeks 3 through 6, where work already exists, your commit sits on top of the
existing one and contains whatever you changed, which may be nearly all of it.

## Rules

### The one that gets the PR rejected

**Change comment lines only.** Never alter a declaration, a body, an import, an
annotation, or a blank line. This is checked mechanically before merge:

```bash
diff <(git show main:$f | grep -vE '^\s*(\*|/\*\*|\*/)' | grep -v '^\s*$') \
     <(grep -vE '^\s*(\*|/\*\*|\*/)' $f | grep -v '^\s*$')
```

Every file must come back identical. Two models under the old pipeline silently
deleted code while filling comments, and every other signal reported success.

### Document the code, not the name

This is the single largest source of defects across six weeks.

Read the implementation of every declaration before documenting it. A method
called `map` on a type that is never completed may be `= this`: nothing is
applied, nothing is created, and prose about "the result of the function" is
false. Week 4 shipped fifteen `@return` tags describing values their methods
cannot produce, and a human reviewer filed 51 comments, roughly 46 of them
factual.

Concretely:

- If the body is `= this`, say it returns itself, and why.
- A method returning `Nothing`, or whose body ends in `throw`, never returns
  normally. Do not give it an `@return`.
- If a parameter is never used by the body, say so on its tag. If it **is** used,
  never say "(ignored)". `Success.fold` shipped with `@param fa ... (ignored)`
  while the body called `fa` on any non-fatal exception from `fb`.
- Check each `@param` name against the signature. `DurationConversions` shipped
  `@param c the classifier instance` on 20 methods where `ev` is the classifier
  and `c` is the value being converted. That one mistake drew 20 identical
  review comments.
- Do not invent exceptions. Week 6 found documentation promising
  `ClassCastException` on methods where erasure and `ClassTag` array-store
  semantics make it impossible, and `ArrayIndexOutOfBoundsException` on a method
  that silently corrupts state instead of throwing.

### Tags

- `@throws` takes the **exception class as its first token**, then the
  condition: `@throws IllegalArgumentException if n is negative`. Scaladoc reads
  that first word as the class name, so `@throws the exception thrown by f`
  renders as an exception called "the".
- Drop `@return` only when the description begins with the word "Returns" and
  already states the whole return value. Otherwise keep it, carrying something
  the description does not say: an edge case, a sentinel, a unit, an exception
  guarantee.
- Never write a literal `@deprecated(...)` line inside a doc comment. The real
  annotation on the declaration is what Scaladoc renders.
- A doc comment must sit **above** any annotation on its declaration. Below it,
  the parser drops the comment entirely.
- Never leave two adjacent `/** ... */` comments on one declaration. The second
  wins and the first is orphaned. Check for an existing comment separated from
  the declaration by `//` comments before adding one.

### Families of near-identical members

The library is full of them: `Function0` through `Function22`, the `Ordering`
and `Numeric` instances, the `jdk` accumulators, 365 wrappers in
`FunctionWrappers.scala`. Two rules:

- **Be consistent within a family.** Write the shared text once and apply it,
  varying only what actually differs.
- **Consistency is not worth a rewrite.** If members are worded differently but
  all correct, leave them. A family worded two ways is untidy; it misleads
  nobody. An automated reviewer spent four rounds demanding "reword this like
  its 40 siblings" and improved nothing.

Prose that is true of the first member of a family is often false of the fifth,
because the members are what differ. Check the shared text against every member
it lands on.

### Voice

- The first sentence stands alone as an API-index summary.
- Declarative, third person, present tense. Not "This method returns ...".
- Backticks for code references, `[[...]]` for links, Markdown over HTML.
- A one-line doc that only spells out the identifier in words adds nothing.
  Either say something a reader could not infer from the name, the unit, the
  reason for a value, what happens at a limit, or leave it for a human.

### When the code does not tell you

Some questions cannot be answered from the source. Do not guess. Leave the
question in the file:

```scala
/** Clears all elements from this accumulator, resetting the internal arrays.
 *
 *  @note NEEDS-HUMAN: Confirm the side effects of `super.clear()`.
 */
```

These are read and resolved by a human before merge. Use the mechanism when it
is genuinely warranted; a marker on every third declaration is noise.

One real example worth imitating, found in week 6:

> `@note NEEDS-HUMAN`: the guard here is `n <= 0` (the size of the current
> block), whereas `AnyAccumulatorStepper` and `IntAccumulatorStepper` guard on
> `N <= 0` (the elements remaining). Is stepping past the end of an exhausted
> stepper meant to be unchecked here?

That required reading three files and may be a real bug rather than a
documentation gap.

## Known defects in the existing work

**Read the human review threads first.** They are the most valuable input you
have, worth more than anything in this brief, because they are a Scala
maintainer saying in their own words what is wrong with documentation of exactly
this kind:

- week 3: https://github.com/scala/scala3/pull/26669
- week 4: https://github.com/scala/scala3/pull/26822 (51 comments)

Read every comment, including the ones that were declined and why. Apply what
you learn across all the weeks you touch, not only to the files being discussed.

Weeks 5 and 6 were never seen by a human, but our own reviewers recorded defect
lists for them. These are a floor, not a ceiling; fix them and whatever else you
find.

**Week 5**, with open blockers recorded at the time:

| file | blockers |
|---|---|
| `collection/concurrent/TrieMap.scala` | 3, including "deep copy" on `INode.copyToGen`, whose body writes the same main-node reference into the new node, and "Returns this TNode" on `TNode.copyTombed`, whose body is `new TNode(...)` |
| `math/ScalaNumericConversions.scala` | 5 |
| `math/Numeric.scala` | 2, including a literal `@deprecated(...)` line inside a doc comment |
| `math/BigDecimal.scala` | 1, a missing `@throws` on a constructor that needs verifying against the code |
| `collection/concurrent/Map.scala` | 1, a malformed `@throws` rendering as an exception named "the" |

**Week 6**: `jdk/FunctionWrappers.scala` has one grouped blocker covering 21
identical `@return` aliases. `quoted/Quotes.scala` (286 declarations, 6208
lines) and four small `quoted` files were never started.

Per-file digests with the full item lists are in `todo-writer/reviews/*.digest.md`.

## Weeks 1 and 2, handled differently

Weeks 1 and 2 are already an open, undrafted pull request, so their branch must
not be rewritten. Instead:

1. Branch **off that PR branch**.
2. Read the documentation it added, and change only what is genuinely wrong or
   materially unclear, judged by the rules above.
3. Commit those improvements as **one separate commit**.

The point is that a human reviewer can see your changes as a distinct, small
diff on top of what they have already reviewed. If nothing is worth changing,
say so and make no commit; that is a legitimate outcome.

## After you finish

For visibility, not for you to do:

1. Codex reviews your work and reports findings **without making changes**.
2. Opus adjudicates those findings.
3. Each of your weekly commits is cherry-picked onto its own branch off `main`
   and becomes one pull request. Weeks 3 and 4 have draft PRs that will be
   force-pushed over.

So your commits are the unit of delivery. Keep them clean, keep them one per
week, and keep every one of them comment-only.

## The short version

Read the code before you describe it. Never touch a non-comment line. One commit
per week, two for week 11. When the code does not tell you, say so rather than
inventing an answer.

And read the two PR threads before you start. The existing documentation in
weeks 3 through 6 is a draft by a weaker writer; you are free to replace all of
it, and the only thing being judged is what a Scala maintainer thinks of the
result.
