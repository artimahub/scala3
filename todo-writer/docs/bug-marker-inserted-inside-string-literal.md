# Bug: `TODO FILL IN` markers get inserted inside string literals

**Status:** fixed
**Found:** 2026-08-14, during the week-4 partition run (util + concurrent)
**Fixed:** 2026-08-16, `lineStartsInsideStringsAndComments` + fixer guard
**Severity:** low frequency, high consequence. One occurrence in 443 declarations,
but it silently edits compiler-visible program text and the existing code-integrity
guard cannot see it.

## Summary

`todo-writer --only-undocumented` scans a Scala source file line by line looking for
declarations that have no preceding Scaladoc block. The scan has no lexical awareness
of string literals, so a line that *looks* like a declaration but is actually text
inside a `"""..."""` string is treated as real, undocumented API and gets a
`/** TODO FILL IN */` stub inserted above it.

The stub lands inside the string. It is not a comment. It changes the string's value.

## The occurrence we hit

`library/src/scala/concurrent/ExecutionContext.scala` carries an `@implicitNotFound`
annotation whose message contains sample code showing the user how to define an
`ExecutionContext`:

```scala
@implicitNotFound("""Cannot find an implicit ExecutionContext. You might add
an (implicit ec: ExecutionContext) parameter to your method.

The ExecutionContext is used to configure how and on which
thread pools asynchronous tasks (such as Futures) will run,
so the specific ExecutionContext that is selected is important.

If your application does not define an ExecutionContext elsewhere,
consider using Scala's global ExecutionContext by defining
the following:

implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global""")
trait ExecutionContext {
```

The line

```scala
implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global""")
```

is sample code inside a diagnostic message. todo-writer read it as an undocumented
`val` and produced:

```scala
consider using Scala's global ExecutionContext by defining
the following:

/** TODO FILL IN */
implicit val ec: scala.concurrent.ExecutionContext = ...global""")
```

The file still compiles, because any text is legal inside a triple-quoted string.
What changes is the error message the compiler prints to users who forget an
`ExecutionContext`. Had the marker been filled rather than left behind, generated
Scaladoc prose would have shipped inside a compiler diagnostic.

## Fix implemented

`findUndocumentedResults` now precomputes, with
`ScaladocChecker.lineStartsInsideStringsAndComments`, the byte offsets of every line
that starts inside a triple-quoted string (`"""..."""`) or a `/* ... */` block
comment, and skips them exactly as it already skipped `//` lines. The scanner is a
single forward pass over the text, so state is carried across line boundaries rather
than inferred per line:

- Tracks `"""..."""` and ordinary `"..."` literals. In ordinary strings `\` escapes
  the next character, and string state resets at end of line.
- Honors the `\"""` escape, which keeps a triple-quote from terminating a
  triple-quoted string.
- Ignores quote characters inside `//` line comments and `/* ... */` block comments
  so they cannot corrupt the string state.

`enclosedInTermMember` (the "is this a local def inside a method body?" check) was
made consistent: it walks line offsets and also skips lines that start inside a
string/comment, so an in-string `val x = 1`-shaped line can no longer masquerade as a
term-member boundary that swallows the real declarations after the string.

Defense in depth: `Fixer` refuses to insert a stub at a line that starts inside a
string literal or block comment, throwing an `IllegalStateException` instead of
silently corrupting the source. If the scan ever regresses, the run fails loudly
rather than leaving a comment-shaped marker inside a string for a writer model to
trip over.

Regression tests live in
`src/test/scala/todowriter/ScaladocCheckerStringLiteralSpec.scala`.

## Reproduction

Both reproductions below were run and confirmed on 2026-08-14 against todo-writer at
`feature-todo-writer` (`d2a746b74d`).

### Against the real file

From a clean checkout of `main` (the file is unmodified upstream):

```bash
cd todo-writer
sbt -batch "run ../library/src/scala/concurrent --only-undocumented"
```

Then look at the annotation:

```bash
sed -n '/@implicitNotFound/,/""")/p' ../library/src/scala/concurrent/ExecutionContext.scala
```

Expected: the annotation is unchanged from upstream, 12 lines.
Actual: a `/** TODO FILL IN */` line has been inserted inside the string, directly
above the `implicit val ec: ...` sample line, making it 13 lines.

The upstream annotation begins at `ExecutionContext.scala:59` on `main` at
`aac9dc1c71`. In our run the inserted marker landed at line 70.

### Minimal reproduction (verified)

Faster than running the whole partition. Put this single file in an otherwise empty
directory:

```scala
import scala.annotation.implicitNotFound

@implicitNotFound("""No Foo found. Define one like this:

implicit val f: Foo = new Foo""")
trait Foo
```

Run:

```bash
cd todo-writer
sbt -batch "run /path/to/that/dir --only-undocumented"
```

todo-writer reports two undocumented declarations where there is only one:

```
File: .../Foo.scala
  At line 5: val f
    - Missing Scaladoc description for an undocumented declaration
  At line 6: trait Foo
    - Missing Scaladoc description for an undocumented declaration

Applied fixes to .../Foo.scala (2 blocks)
```

`val f` at line 5 does not exist. It is text inside the annotation's message string.
The resulting file:

```scala
import scala.annotation.implicitNotFound

@implicitNotFound("""No Foo found. Define one like this:

/** TODO FILL IN */
implicit val f: Foo = new Foo""")
/** TODO FILL IN */
trait Foo
```

The marker at line 7 is correct. The one at line 5 is inside the string and changes
the value of the diagnostic message.

## Root cause

`todo-writer/src/main/scala/todowriter/ScaladocChecker.scala`, in
`findUndocumentedResults`. The scan walks raw text one line at a time:

```scala
var pos = 0
while pos < textLen do
  val lineStart = pos
  val lineEnd   = text.indexOf('\n', pos)
  val lineContent = ...
  pos = if lineEnd >= 0 then lineEnd + 1 else textLen

  if !coveredLineStarts.contains(lineStart) then
    val trimmed = lineContent.trim
    if trimmed.nonEmpty && !trimmed.startsWith("//") &&
       !trimmed.startsWith("*") && !trimmed.startsWith("/*") then
      val chunk = Declaration.getDeclarationAfter(text, lineStart)
      val decl  = Declaration.parse(chunk)
      ...
```

There is state for "is this line a comment" (the `//`, `*`, `/*` prefix tests) but no
state for "am I inside a string literal". Detection throughout todo-writer is
regex-and-prefix based over raw text rather than token based, so any construct that
embeds Scala-looking source inside a string is a candidate for this bug:

- `@implicitNotFound("""...""")` and `@implicitAmbiguous`
- `@deprecated` / `@migration` messages that show replacement code
- doctest or example strings
- error-message templates in macro code

## Why the code-integrity guard did not catch it

The fill pipeline verifies after every file that the non-comment lines of the result
are byte-identical to the baseline. It computes that by stripping comment-shaped
lines from both sides:

```bash
grep -vE '^\s*(\*|/\*\*|\*/)' "$f" | grep -v '^\s*$'
```

The inserted line is `/** TODO FILL IN */` at column 0. It matches the strip pattern,
so it is removed from the candidate side before comparison, and the baseline never had
it. The two sides compare equal and the guard reports PASS.

In other words: an insertion into a string literal is invisible to the guard precisely
because the inserted text is comment-shaped. The guard protects against code being
deleted or moved, not against comment-shaped text being added somewhere that is not a
comment.

This one surfaced only as a side effect. The writer model could not produce a
SEARCH/REPLACE block that matched, stalled after three passes, and the "no progress"
warning is what sent us looking.

## Blast radius

Checked every marker introduced so far. A scanner that tracks triple-quoted and
ordinary string state (see below) reports:

| scope | in-string markers |
|---|---|
| week-4 marking commit `5dcf87776e`, 36 files, 443 declarations | 1 (this one) |
| the 15 week-4 files not yet filled, 1007 markers | 0 |
| PR branch `scaladoc-missing-docs-core-array-function-tuple-sys`, 108 files | 0 |
| PR branch `missing-docs-root-array-predef-root-function-tuple-sys`, 126 files | 0 in `library/src` |

The second PR branch shows 60 hits, all in todo-writer's own test specs
(`FixerSpec.scala`, `LinkCheckerSpec.scala`, `ScaladocCheckerLinkSpec.scala`,
`WikidocToMarkdownSpec.scala`), where Scaladoc inside triple-quoted strings is the
fixture data and is meant to be there. Nothing shipped is affected.

## Detection script

Useful as a pre-commit check until the parser is fixed. Reports markers that sit
inside a string literal, handling both `"""..."""` and ordinary `"..."` with escapes:

```python
import pathlib

def markers_in_strings(src):
    hits = []; i = 0; n = len(src); in3 = False; line = 1; instr1 = False
    while i < n:
        c = src[i]
        if c == '\n':
            line += 1; instr1 = False; i += 1; continue
        if not instr1 and src.startswith('"""', i):
            in3 = not in3; i += 3; continue
        if not in3:
            if c == '\\' and instr1: i += 2; continue
            if c == '"': instr1 = not instr1; i += 1; continue
        if (in3 or instr1) and src.startswith('TODO FILL IN', i):
            hits.append(line)
        i += 1
    return hits

for f in sorted(pathlib.Path("library/src/scala").rglob("*.scala")):
    src = f.read_text()
    if "TODO FILL IN" not in src:
        continue
    for ln in markers_in_strings(src):
        print(f"{f}:{ln}  marker inside a string literal")
```

## Fix direction

The real fix was to give the undocumented-declaration scan string-literal state, so a
line inside `"""..."""` or `"..."` is skipped the same way a `//` line is. That meant
tracking quote state as the scan advances through the text, in the same loop that
already tracks line boundaries, rather than testing each line in isolation. This is
done (see [Fix implemented](#fix-implemented)).

Two things were worth deciding at the same time:

1. **The integrity guard should not be comment-blind.** Consider having it compare
   against the *pre-marking* baseline including string contents, or add a separate
   check that string literals are unchanged. As written it structurally cannot catch
   this class of damage. *Still open: this lives in the fill pipeline's scripts, not
   in todo-writer's code.*

2. **Markers inside strings should be an error, not a silent skip.** If the scan ever
   does produce one, it is better to fail the marking run loudly than to leave it for
   a writer model to trip over several days later. *Done: the fixer throws rather
   than insert at a line that starts inside a string literal.*

## Workaround applied

For week 4 the marker was deleted by hand and the annotation verified byte-identical
to pre-marking upstream:

```bash
git show 5dcf87776e^:library/src/scala/concurrent/ExecutionContext.scala \
  | sed -n '/@implicitNotFound/,/""")/p' > /tmp/up
sed -n '/@implicitNotFound/,/""")/p' library/src/scala/concurrent/ExecutionContext.scala > /tmp/now
diff /tmp/up /tmp/now && echo identical
```

The file's 7 genuine declarations were documented normally. The false one was removed,
not filled.
