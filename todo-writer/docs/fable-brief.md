# Brief: finish the Scala 3 standard library Scaladoc rollout

## Why this work exists

Scala 3's standard library is mature and carefully built. Its documentation does
not say so. Thousands of public declarations carry no Scaladoc at all, and a
developer browsing the API reference meets one blank entry after another.

That does two kinds of harm, and the second is the less obvious one.

The practical harm is the one you would expect. Someone trying to use
`Accumulator`, `TrieMap` or `Ordering` cannot find out what a method does, what
it returns at the edges, or what it throws, without stopping to read the source.
Every gap is a small tax on every person who hits it, forever.

The other harm is to how Scala is perceived. An API reference full of holes
reads as unfinished, as something not yet ready for production use. Scala is not
that, and the gaps misrepresent it. Part of the point of this work is
straightforwardly presentational: to make the language look as finished as it
actually is. That is a real goal, not a lesser one.

Both goals set the same quality bar, and it is higher than "something in every
box". Documentation that is confidently wrong serves neither: it misleads the
person who trusts it, and to anyone who checks it against the code, it looks
worse than saying nothing would have. A blank entry is an omission. A false one
is a defect, and it is the kind of defect that makes a library look careless
rather than merely incomplete.

So the standard throughout is: true first, useful second, complete third. If you
cannot make an entry true, leave it undone and say why. There are instructions
below for exactly that.

## The task

You are writing the missing Scaladoc for the Scala 3 standard library, weeks 3
through 11 of an 11-week plan. Weeks 1 and 2 are already in an open pull request
and are handled separately at the end of this brief.

**"Week" is a name, not a schedule.** The original plan was one pull request per
calendar week, and the label stuck as the name for a slice of the library. It
carries no time budget and implies no pacing. Nine "weeks" does not mean nine
weeks of work, or nine of anything; it means nine partitions, each of which
becomes one pull request. Take whatever time each one actually needs and work
at whatever rate you work.

**Why it is split at all.** The whole job is roughly 8,360 undocumented
declarations. As a single pull request that is unreviewable: nobody can read it
and responsibly say yes, so it would sit unmerged or be waved through, and
either outcome wastes the work. The partitions exist for one reason, to make
each pull request small enough that a human can review it properly in a
reasonable sitting, which in practice means a few hundred declarations of new
prose. That is also why a commit must never span two weeks: the commit *is* the
pull request.

Worth remembering who is at the other end of it. The reviewer is a Scala
maintainer spending their own time reading your prose against their library. On
week 4 they typed essentially the same comment nineteen times, because one wrong
`@param` had been copied across a family of methods. Every defect you avoid is
time they get back; every one you leave is time they spend, and goodwill this
project needs for another eight pull requests.

Everything below is drawn from six weeks of doing this with an automated
pipeline, which is no longer in charge of this work. You are.

That means none of its process binds you; how you work is your call entirely.
What does bind you is the output: comment-only changes, one commit per week, and
documentation that is true of the code. Between those two sits everything else
below, the mistakes that pipeline actually made. They are not procedure and they
are not optional; they are the ways this particular job goes wrong, and they go
wrong the same way whoever is doing it.

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

- `scripts/repeated-doc-blocks.py --orig X --new Y` groups doc blocks and tag
  lines you have written more than once, which is how you check a family for a
  mistake replicated across 40 members.
- `scripts/fill-doc-todos-free.sh` is the pipeline that produced weeks 3 through
  6. **It did not quite work**, which is why this job came to you. Look at it if
  you are curious; do not expect it to do the work for you.

  Why it fell short is worth knowing, because it says something about what this
  job demands. An earlier phase of this project filled in missing `@param`,
  `@tparam` and `@return` tags on declarations that *already had* documentation.
  That is local, bounded analysis: the answer is nearly always in the signature
  and the sentence above it, and a modest model does it well. Writing a
  description from nothing is a different task. It means reading the body,
  following an override to the member it overrides, checking a sibling in
  another file, and judging what a caller actually needs to be told. That asks
  for a broader view than those models could hold, and the results show it: the
  defect lists later in this brief are what a narrower view produces.
- `reviews/*.digest.md` holds every review of weeks 4 through 6, including the
  findings that were never applied.
- `docs/house-rules.md` is a short list of conventions distilled from earlier
  feedback. Append to it if you learn something worth passing on.

The constraints that follow are about the OUTPUT, not the method: comment-only
changes, one commit per week, and documentation that is true of the code.

**Do one good pass and stop.** Do not run review passes over your own work, and
do not build a review loop out of the tooling above. Getting a second opinion on
your own output is not your job and it is expensive; review happens after you,
by other models, as described at the end of this brief. Spend your effort on the
first pass being right rather than on checking it afterwards.

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
- **Rewording for its own sake is not an improvement.** If two members say the
  same true thing in different words, and both are clear, that is fine. An
  automated reviewer spent four rounds demanding "phrase this like its 40
  siblings" and improved nothing while risking a new false claim with every
  rewrite. Leave those alone.

But notation is a different matter, and it cuts the other way.

Prose that is true of the first member of a family is often false of the fifth,
because the members are what differ. Check the shared text against every member
it lands on.

### Notation, and the one time to touch existing comments

Two different things get called consistency and they deserve opposite answers.
Wording is the one above: leave it. **Notation is not, and you should make it
uniform everywhere.**

The clearest example is real, from this project's own output: some comments say
"the second element" and others say "the 2nd element". Nobody is misled by
either. But that unevenness is exactly what makes a library look unfinished, and
looking finished is half the reason this work exists. Spelling out an ordinal in
one comment and using a numeral in the next is the documentation equivalent of
mismatched indentation.

The same goes for any notational choice the library makes repeatedly: how types
are referred to, whether code names are in backticks, capitalisation of tag
continuations, how a sentence in a tag is punctuated. Find the form the library
predominantly uses, and use that form.

**Do it across files, not just within one.** The inconsistency exists because the
pipeline that wrote weeks 3 through 6 was handed one file at a time and could
not see that `Vector` and `List` had made different choices. You can hold the
whole library at once. That is one of the reasons this job came to you, and it
is work only you can do.

**This is the one case where you should edit documentation that predates this
project.** If an existing comment uses "2nd" and the library's prevailing form is
"second", change it. Otherwise leave pre-existing comments alone: they are not
your remit, they have already been reviewed by someone, and every one you touch
enlarges the diff a maintainer has to check. Notational consistency is worth
that cost because it is invisible work that makes the whole library read as one
thing. Rewriting someone else's correct prose is not.

### Voice

The official style guide is reproduced in full in the appendix, and it is the
authority on everything in this section. Read it. In particular it asks for
"Returns XXX" as the opening of a method's summary, `[[scala.Option]]` links,
wiki syntax over HTML, and the present indicative throughout ("returns the
result of applying f to x", not "return the result").

Comment formatting: this project uses one of the three styles the guide permits,
gutter asterisks aligned in column two, text on the same line as `/**`:

```scala
/** Returns the first element of this list.
 *
 *  @return the first element
 *  @throws NoSuchElementException if this list is empty
 */
```

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

### When the code is wrong

Different from the case above, and more important. Sometimes the code does not
merely fail to reveal its intent, it is plainly wrong: an off-by-one, a guard on
the wrong variable, a branch that cannot be reached. This will happen. You are
about to read several thousand declarations more carefully than anyone has in
years.

Three rules, in order:

1. **Do not document the buggy behaviour as though it were intended.** A doc
   comment faithfully describing a bug makes the bug permanent: it turns a
   defect into a specified contract, and the next reader has no way to tell
   which it is.
2. **Do not fix it.** These pull requests are comment-only, and that property is
   what lets a reviewer accept 900 declarations without auditing them line by
   line. A code fix buried in a documentation PR costs that trust and stalls the
   whole thing.
3. **Report it, in two places.** Leave a `@note NEEDS-HUMAN:` at the site saying
   what looks wrong, and add an entry to `todo-writer/docs/suspected-bugs.md`
   so they can be read in one place without hunting through the diff.

For the declaration itself, either leave the marker unfilled with the
NEEDS-HUMAN note explaining why, or document only what is unambiguously true and
flag the rest. Do not guess which behaviour was meant.

This is a proven path, not a formality: a bug found this way a few weeks ago was
raised as its own small pull request and accepted immediately. Separating the
two kinds of change is what made that quick.

Format for `suspected-bugs.md`, one entry each:

```markdown
## `collection/concurrent/TrieMap.scala:1204` — `DoubleAccumulatorStepper.hasStep`

The guard is `n <= 0`, the size of the current block, whereas
`AnyAccumulatorStepper` and `IntAccumulatorStepper` guard on `N <= 0`, the
elements remaining. Stepping past the end of an exhausted stepper therefore
appears to be unchecked here and checked in the siblings.

Not documented; `@note NEEDS-HUMAN` left at the site.
```

Include what the code does, what the siblings or the contract suggest it should
do, and the evidence for both. Someone who has not read the file needs to be
able to judge it from your entry alone.

## Known defects in the existing work

**Read the human review comments first.** They are the most valuable input you
have, worth more than anything in this brief, because they are a Scala
maintainer saying in their own words what is wrong with documentation of exactly
this kind.

All of them are captured in **`todo-writer/docs/human-review-comments.md`**, so
you do not need network access: 8 comments on week 3 and 51 on week 4, with the
diff each one points at, the author's replies, and an editor's note explaining
the four that were declined and why. The originals are at
<https://github.com/scala/scala3/pull/26669> and
<https://github.com/scala/scala3/pull/26822>.

Read all of it, including the declined comments; the reasoning there shows that
a maintainer's comment can be wrong and what it takes to say so. Apply what you
learn across every week you touch, not only to the files being discussed. Note
also how the comments cluster: 19 of week 4's 51 were one mistake replicated
across a family, and 15 more were another.

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
inventing an answer. When the code is wrong, do not document it and do not fix
it: write it down in `suspected-bugs.md` and leave it for a separate PR.

And read the two PR threads before you start. The existing documentation in
weeks 3 through 6 is a draft by a weaker writer; you are free to replace all of
it, and the only thing being judged is what a Scala maintainer thinks of the
result.

---

# Appendix: the official Scaladoc style guide

Reproduced verbatim from <https://docs.scala-lang.org/style/scaladoc.html>, the
official Scala documentation style guide, in case you cannot reach the page.

**One edit:** the guide shows three acceptable comment styles. This project uses
exactly one of them, *Scaladoc style with gutter asterisks aligned in column
two*, which is what you will find throughout the library. The other two examples
have been omitted so they are not mistaken for options. Everything else is as
published.

> It is important to provide documentation for all packages, classes, traits,
> methods, and other members. Scaladoc generally follows the conventions of
> Javadoc, but provides many additional features that simplify writing
> documentation for Scala code.
>
> In general, you want to worry more about substance and writing style than
> about formatting. Scaladocs need to be useful to new users of the code as well
> as experienced users. Achieving this is very simple: increase the level of
> detail and explanation as you write, starting from a terse summary (useful for
> experienced users as reference), while providing deeper examples in the
> detailed sections (which can be ignored by experienced users, but can be
> invaluable for newcomers).
>
> The Scaladoc tool does not mandate a documentation comment style.

**The style this project uses**, a single line summary followed by detailed
documentation, with gutter asterisks aligned in column two:

```scala
/** Provides a service as described.
 *
 *  This is further documentation of what we're documenting.
 *  Here are more details about how it works and what it does.
 */
def member: Unit = ()
```

> Because the comment markup is sensitive to whitespace, the tool must be able
> to infer the left margin.
>
> When only a simple, short description is needed, a one-line format can be
> used:

```scala
/** Does something very simple */
def simple: Unit = ()
```

> Note that, in contrast to the Javadoc convention, the text in the Scaladoc
> styles begins on the first line of the comment. This format saves vertical
> space in the source file.
>
> In either Scaladoc style, all lines of text are aligned on column five. Since
> Scala source is usually indented by two spaces, the text aligns with source
> indentation in a way that is visually pleasing.

## General Style

> It is important to maintain a consistent style with Scaladoc. It is also
> important to target Scaladoc to both those unfamiliar with your code and
> experienced users who just need a quick reference. Here are some general
> guidelines:
>
> - Get to the point as quickly as possible. For example, say "returns true if
>   some condition" instead of "if some condition return true".
> - Try to format the first sentence of a method as "Returns XXX", as in
>   "Returns the first element of the List", as opposed to "this method returns"
>   or "get the first" etc. Methods typically return things.
> - This same goes for classes; omit "This class does XXX"; just say "Does XXX"
> - Create links to referenced Scala Library classes using the square-bracket
>   syntax, e.g. `[[scala.Option]]`
> - Summarize a method's return value in the `@return` annotation, leaving a
>   longer description for the main Scaladoc.
> - If the documentation of a method is a one line description of what that
>   method returns, do not repeat it with an `@return` annotation.
> - Document what the method does do not what the method should do. In other
>   words, say "returns the result of applying f to x" rather than "return the
>   result of applying f to x". Subtle, but important.
> - When referring to the instance of the class, use "this XXX", or "this" and
>   not "the XXX". For objects, say "this object".
> - Make code examples consistent with this guide.
> - Use the wiki-style syntax instead of HTML wherever possible.
> - Examples should use either full code listings or the REPL, depending on what
>   is needed (the simplest way to include REPL code is to develop the examples
>   in the REPL and paste it into the Scaladoc).
> - Make liberal use of `@macro` to refer to commonly-repeated values that
>   require special formatting.

## Packages

> Provide Scaladoc for each package. This goes in a file named `package.scala`
> in your package's directory and looks like so (for the package
> `parent.package.name.mypackage`):

```scala
package parent.package.name

/** This is the Scaladoc for the package. */
package object mypackage {
}
```

> A package's documentation should first document what sorts of classes are part
> of the package. Secondly, document the general sorts of things the package
> object itself provides.
>
> While package documentation doesn't need to be a full-blown tutorial on using
> the classes in the package, it should provide an overview of the major
> classes, with some basic examples of how to use the classes in that package.
> Be sure to reference classes using the square-bracket notation:

```scala
package my.package
/** Provides classes for dealing with complex numbers.  Also provides
 *  implicits for converting to and from `Int`.
 *
 *  ==Overview==
 *  The main class to use is [[my.package.complex.Complex]], as so
 *  {{{
 *  scala> val complex = Complex(4,3)
 *  complex: my.package.complex.Complex = 4 + 3i
 *  }}}
 *
 *  If you include [[my.package.complex.ComplexConversions]], you can
 *  convert numbers more directly
 *  {{{
 *  scala> import my.package.complex.ComplexConversions._
 *  scala> val complex = 4 + 3.i
 *  complex: my.package.complex.Complex = 4 + 3i
 *  }}}
 */
package complex {}
```

## Classes, Objects, and Traits

> Document all classes, objects, and traits. The first sentence of the Scaladoc
> should provide a summary of what the class or trait does. Document all type
> parameters with `@tparam`.

### Classes

> If a class should be created using its companion object, indicate as such
> after the description of the class (though leave the details of construction
> to the companion object). Unfortunately, there is currently no way to create a
> link to the companion object inline, however the generated Scaladoc will
> create a link for you in the class documentation output.
>
> If the class should be created using a constructor, document it using the
> `@constructor` syntax:

```scala
/** A person who uses our application.
 *
 *  @constructor create a new person with a name and age.
 *  @param name the person's name
 *  @param age the person's age in years
 */
class Person(name: String, age: Int) {
}
```

> Depending on the complexity of your class, provide an example of common usage.

### Objects

> Since objects can be used for a variety of purposes, it is important to
> document how to use the object (e.g. as a factory, for implicit methods). If
> this object is a factory for other objects, indicate as such here, deferring
> the specifics to the Scaladoc for the `apply` method(s). If your object doesn't
> use `apply` as a factory method, be sure to indicate the actual method names:

```scala
/** Factory for [[mypackage.Person]] instances. */
object Person {
  /** Creates a person with a given name and age.
   *
   *  @param name their name
   *  @param age the age of the person to create
   */
  def apply(name: String, age: Int) = {}

  /** Creates a person with a given name and birthdate
   *
   *  @param name their name
   *  @param birthDate the person's birthdate
   *  @return a new Person instance with the age determined by the
   *          birthdate and current date.
   */
  def apply(name: String, birthDate: java.time.LocalDate) = {}
}
```

> If your object holds implicit conversions, provide an example in the Scaladoc:

```scala
/** Implicit conversions and helpers for [[mypackage.Complex]] instances.
 *
 *  {{{
 *  import ComplexImplicits._
 *  val c: Complex = 4 + 3.i
 *  }}}
 */
object ComplexImplicits {}
```

### Traits

> After the overview of what the trait does, provide an overview of the methods
> and types that must be specified in classes that mix in the trait. If there
> are known classes using the trait, reference them.

## Methods and Other Members

> Document all methods. As with other documentable entities, the first sentence
> should be a summary of what the method does. Subsequent sentences explain in
> further detail. Document each parameter as well as each type parameter (with
> `@tparam`). For curried functions, consider providing more detailed examples
> regarding the expected or idiomatic usage. For implicit parameters, take
> special care to explain where these parameters will come from and if the user
> needs to do any extra work to make sure the parameters will be available.
