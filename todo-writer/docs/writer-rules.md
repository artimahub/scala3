# Rules for writing Scaladoc in the Scala 3 standard library

You are writing missing Scaladoc for the Scala 3 standard library, inside
`/workspace/scala3`. Declarations needing documentation carry `TODO FILL IN`
markers. Your job on your assigned file(s): replace every marker with real,
accurate documentation. The result becomes a pull request reviewed by a Scala
maintainer. The bar, in order: true first, useful second, complete third.

## The rule that gets the PR rejected

**Change comment lines only.** Never alter a declaration, a body, an import, an
annotation, or a blank line. Never add or remove blank lines. This is checked
mechanically:

```bash
f=library/src/scala/Foo.scala
diff <(git show main:$f | grep -vE '^\s*(\*|/\*\*|\*/)' | grep -v '^\s*$') \
     <(grep -vE '^\s*(\*|/\*\*|\*/)' $f | grep -v '^\s*$')
```

Must print nothing for every file you touch. Run it on each of your files
before you finish. Use the Edit tool for all changes (never sed). Never run
any git command that writes: no add, commit, checkout, reset, stash, restore.
Git reads (show, diff, log) are fine.

## Document the code, not the name

This is the single largest source of real defects in this project. Read the
implementation of every declaration before documenting it. Follow an override
to the member it overrides; check a sibling file when the member delegates.
Concretely:

- If the body is `= this`, say it returns itself, and why. Never write
  "creates a new X by applying f" when nothing is created and nothing applied.
- A method returning `Nothing`, or whose body always throws, never returns
  normally. Give it no `@return`; say it always throws, with `@throws`.
- If a parameter is never used by the body, say so on its tag ("never used",
  "never called"). If it IS used — on any path, including catch blocks — never
  say it is ignored. (`Success.fold` shipped with "`fa` ... (ignored)" while
  the body calls `fa` on a non-fatal exception from `fb`. A reviewer caught it.)
- Check each `@param` name against the signature. One wrong `@param` copied
  across a 20-member family drew 20 identical review comments.
- Do not invent exceptions. Never promise `ClassCastException` where erasure
  makes it impossible, or an exception on a code path that silently does
  something else instead. Only document a throw you can point to in the code
  (or in the documented contract of the thing delegated to).
- Never describe behaviour that cannot occur for the receiver you are
  documenting (e.g., "when this future completes" on a future that never
  completes).

## Tags

- `@throws` takes the **exception class as its first token**, then the
  condition: `@throws IllegalArgumentException if n is negative`. Scaladoc
  renders the first word as the class name, so `@throws the exception thrown
  by f` renders as an exception named "the".
- Prefer `@throws` tags over exception prose in the description.
- Drop `@return` (delete the marker line) only when the description begins
  with "Returns" and already states the whole return value. Otherwise keep it,
  carrying something the description does not say: an edge case, a sentinel, a
  unit, an exception guarantee. Never let `@return` restate the description
  verbatim, and never let a tag contradict the description.
- Document every type parameter with `@tparam` where the marker asks for it.
- Never write a literal `@deprecated(...)` line inside a doc comment, and do
  not restate a `@deprecated` annotation in prose; Scaladoc renders the real
  annotation itself.
- A doc comment must sit **above** any annotation on its declaration.
- Never leave two adjacent `/** ... */` comments on one declaration; check for
  an existing doc comment (possibly separated by `//` lines) before the
  declaration you are documenting. If a marker sits below an existing doc
  comment for the same declaration, merge: fill the content into ONE comment
  (this should not happen — markers were placed only on undocumented
  declarations — but check).
- Tag order: `@tparam`, then `@param`, then `@return`, then `@throws`. Other
  tags (`@note`, `@see`, `@example`) after.

## Style and voice

Comment format — gutter asterisks aligned in column two (relative to the
comment's indentation), text starting on the `/**` line:

```scala
  /** Returns the first element of this list.
   *
   *  Further detail if warranted.
   *
   *  @return the first element
   *  @throws NoSuchElementException if this list is empty
   */
```

One-line form for simple members: `/** Does something very simple */` (with or
without trailing period; prefer a period on full sentences).

- The first sentence stands alone as an API-index summary. Get to the point.
- Methods: open with "Returns XXX" where the method returns something; classes
  and traits: "Does XXX" / "A [thing that] ...", never "This class does".
- Declarative, third person, present indicative: "returns", "applies", never
  "return", "will return", "this method returns".
- When referring to the instance: "this list", "this option", "this". For
  objects: "this object".
- Backticks for code references, `[[scala.Option]]` square-bracket links for
  Scala library classes, wiki/Markdown syntax over HTML, `{{{ ... }}}` for
  code blocks.
- No hedging that carries no information ("is to be considered non-fatal" ->
  "is non-fatal").
- A doc that only spells out the identifier in words adds nothing. Say
  something the reader cannot infer from the name: the unit, the edge case,
  the sentinel, why the value exists, what happens at a limit. This matters
  most on `val`s and simple accessors.
- Do not write an em-dash (U+2014) anywhere; use a hyphen or reword.

## Families of near-identical members

`Function0`-`Function22`, `Tuple1`-`Tuple22`, ordering/numeric instances,
wrappers: write the shared text once and apply it, varying only what actually
differs. Check that the shared text is true of EVERY member it lands on; prose
true of the first member is often false of the fifth. Do not reword existing
docs of siblings for its own sake if they are true and clear.

**Ordinals:** in the arity families (`FunctionN`/`TupleN`/`ProductN`),
positional elements take the NUMERAL ordinal: "the 1st element", "the 2nd
element". Ordinary prose elsewhere keeps the spelled form ("the first element
of this list").

## Pre-existing documentation is off limits

Documentation that predates this project (i.e., is unchanged relative to
`main`) must not be edited, with ONE exception: change a spelled ordinal
("first") to the numeral form ("1st") when it sits in the same family or tag
list you are already documenting AND is inconsistent with its immediate
neighbours there. Nothing else — not typos, not wrong prose, not other
inconsistencies. If you see something wrong in pre-existing docs, report it in
your final message under "consistency notes" instead of fixing it.

Documentation ADDED by this project (visible as `+` lines in
`git diff main -- <file>` that are not markers) is a draft by a weaker writer.
If your task prompt says to revise the file, check that draft against the code
and rewrite whatever is false, misleading, or below the bar above. True and
clear prose stays even if you would have phrased it differently.

## When the code does not tell you

Do not guess. Leave the question in the file:

```scala
  /** Clears all elements from this accumulator, resetting the internal arrays.
   *
   *  @note NEEDS-HUMAN: Confirm the side effects of `super.clear()`.
   */
```

Use it when genuinely warranted; a marker on every third declaration is noise.

## When the code is wrong

You will occasionally find a real bug: an off-by-one, a guard on the wrong
variable, a doc-contract violation. Three rules, in order:

1. Do not document the buggy behaviour as though intended.
2. Do not fix it (comment-only PR).
3. Report it: leave `@note NEEDS-HUMAN: <what looks wrong>` at the site,
   document only what is unambiguously true, and include a full entry in your
   final message under "SUSPECTED BUGS" with: file:line, member, what the code
   does, what siblings/contract suggest it should do, and the evidence.
   Someone who has not read the file must be able to judge it from your entry.

## What to report back

Your final message must contain, briefly:

1. Files completed, and for each: markers remaining (must be 0), comment-only
   check result (must be clean).
2. Any `@note NEEDS-HUMAN` left, one line each (file:line + question).
3. "SUSPECTED BUGS" entries, full format as above (or "none").
4. "CONSISTENCY NOTES": pre-existing-doc inconsistencies you noticed but did
   not touch (or "none").

Do not write to any file outside your assigned source files. No summary
documents, no edits under todo-writer/.
