# TodoWriter: Scaladoc Tag Checker and Fixer

## Overview

TodoWriter is a Scala 3 tool that validates Scaladoc documentation comments against code declarations and inserts TODO placeholders for missing documentation tags. It is a Scala 3 reimplementation of the Python `scaladoc_checker.py` script with integrated indentation normalization.

## Purpose

1. Scan Scala source files for Scaladoc comments (`/** ... */`)
2. Validate that `@param`, `@tparam`, and `@return` tags match the associated declaration
3. Report missing or extraneous tags
4. With `--fix`, insert `TODO FILL IN` placeholders for missing tags
5. When modifying a Scaladoc block, normalize its indentation to the canonical style

## Scaladoc Indentation Rules

The tool enforces these indentation rules **only when modifying a block**:

### Single-line Scaladoc
Preserved as-is:
```scala
/** This is just one line of Scaladoc. */
```

### Multi-line Scaladoc
The initial text of the doc comment appears on the same line as `/**`, after a space.
The `*` on continuation lines aligns directly beneath the first `*` of `/**`.
Text on continuation lines starts after two spaces following the `*`.
A blank line (` *`) should appear before any tags.
Tags appear in order: `@see`, then `@tparam`, then `@param`, then `@return`, with no blank lines between them:
```scala
/** First line of comment appears on same line as /**.
 *
 *  Additional description lines.
 *  More details here.
 *
 *  @see [[OtherClass]]
 *  @tparam T the type parameter
 *  @param x description
 *  @return description
 */
```

If `/**` starts at column 4:
```scala
    /** First line of comment on same line.
     *
     *  Second line - the * is in column 5
     *
     *  @tparam A type param
     *  @param x description
     */
```

**Important formatting rules for the Fixer:**
1. **Preserve initial text position**: If the original Scaladoc has text on the `/**` line, keep it there
2. **Move misplaced initial text**: If the original has `/**` alone on a line followed by ` * Initial text`, move the text to the `/**` line
3. **Blank line before tags**: Always ensure a blank ` *` line exists before the first tag
4. **Tag ordering**: Tags must appear in this order: `@see` first, then `@tparam`, then `@param`, then `@return`
5. **No blank lines between tags**: Tags should appear consecutively with no blank lines between them
6. **Don't add spurious blank lines**: Don't insert blank lines above existing content
7. **TODO placeholder**: Missing tags are inserted with `TODO FILL IN` as the description

## Validation Rules

### @param
- Applies to: `def`, `class`, `trait`
- For `def`: documents method parameters
- For `class`/`trait`: documents constructor (primary) parameters
- Each declared parameter should have a corresponding `@param` tag
- Each `@param` tag should reference an actual parameter name
- Reports: "Missing @param for: x, y" or "@param refers to unknown params: z"

### @tparam
- Applies to: `def`, `class`, `trait`
- For `def`: documents method type parameters
- For `class`/`trait`: documents type parameters of the class/trait itself
- Each declared type parameter should have a corresponding `@tparam` tag
- Each `@tparam` tag should reference an actual type parameter name
- Reports: "Missing @tparam for: A, B" or "@tparam refers to unknown type params: C"

### @return
- Applies to: `def` only (not applicable to `class`, `trait`, `object`, `val`, `var`)
- For `def` with non-Unit return type: `@return` should be present
- For `def` with Unit return type: `@return` should be absent
- **One-liner exception**: If the Scaladoc has only a single line of descriptive content (ignoring tags like `@param`, `@tparam`), do NOT add `@return`. Per the Scaladoc style guide: "If the documentation of a method is a one line description of what that method returns, do not repeat it with an @return annotation."
- Reports: "Missing @return for non-Unit return type" or "@return present but return type is Unit"

#### One-liner Examples

These are considered one-liners and should NOT get `@return TODO` added:
```scala
/** Returns the current count. */
def count: Int = ???

/** The name of this entity */
def name: String = ???

/** Gets the value for the given key.
 *
 *  @param key the lookup key
 */
def get(key: String): Option[Int] = ???
```

These are NOT one-liners (multiple lines of descriptive content) and SHOULD get `@return TODO` if missing:
```scala
/** Computes the result.
 *
 *  This method performs complex calculation.
 */
def compute: Int = ???

/** Retrieves the item from the cache.
 *
 *  If the item is not found, returns None.
 *
 *  @param key the cache key
 */
def getFromCache(key: String): Option[Item] = ???
```

## Architecture

### Project Structure

```
todo-writer/
├── build.sbt
├── project/
│   └── build.properties
├── src/
│   ├── main/
│   │   └── scala/
│   │       └── todowriter/
│   │           ├── Main.scala              # CLI entry point
│   │           ├── ScaladocChecker.scala   # Main orchestration logic
│   │           ├── ScaladocBlock.scala     # Scaladoc parsing and tag extraction
│   │           ├── Declaration.scala       # Declaration parsing
│   │           ├── Issue.scala             # Issue types and reporting
│   │           └── Fixer.scala             # Fix application with indentation
│   └── test/
│       └── scala/
│           └── todowriter/
│               ├── ScaladocBlockSpec.scala
│               ├── DeclarationSpec.scala
│               ├── ScaladocCheckerSpec.scala
│               ├── FixerSpec.scala
│               └── IntegrationSpec.scala
└── plan.md
```

### Core Components

#### 1. `ScaladocBlock`
Represents a parsed Scaladoc comment:
- Extracts `@param`, `@tparam`, `@return` tags
- Tracks source location (start/end indices, line number)
- Preserves original content for comparison
- Detects whether the Scaladoc is a "one-liner" (single sentence, no tags) for the `@return` exception

#### 2. `Declaration`
Represents a parsed Scala declaration:
- Kind: `def`, `class`, `trait`, `object`, `val`, `var`
- Name
- Type parameters (names only)
- Value parameters (names only)
- Return type (for `def`)

Uses regex-based heuristic parsing (same approach as Python script).

#### 3. `Issue`
ADT representing validation issues:
```scala
enum Issue:
  case MissingParam(names: List[String])
  case UnknownParam(names: List[String])
  case MissingTparam(names: List[String])
  case UnknownTparam(names: List[String])
  case MissingReturn
  case UnnecessaryReturn
```

#### 4. `ScaladocChecker`
Main orchestration:
- Finds Scala files recursively
- Extracts Scaladoc blocks and following declarations
- Validates tags against declarations
- Collects issues per file

#### 5. `Fixer`
Applies fixes to source files:
- Inserts missing `@param`, `@tparam`, `@return` tags with TODO placeholders
- Respects the one-liner exception: does NOT insert `@return TODO` for single-sentence Scaladocs with no existing tags
- Normalizes indentation of modified blocks only
- Preserves content inside code blocks (`{{{...}}}`, `<pre>...</pre>`)

#### 6. `Main`
CLI interface using `scopt` or manual argument parsing:
```
Usage: todo-writer <folder> [options]
  --fix       Apply fixes to source files
  --json      Output results as JSON
```

## Implementation Plan

### Phase 1: Project Setup
1. Create `build.sbt` with Scala 3, ScalaTest dependencies
2. Create `project/build.properties`
3. Set up basic Main.scala with argument parsing

### Phase 2: Parsing
1. Implement `ScaladocBlock` - regex-based extraction of `/** ... */` blocks
2. Implement tag extraction (`@param`, `@tparam`, `@return`)
3. Implement `Declaration` - heuristic parsing of Scala declarations
4. Handle edge cases: annotations, modifiers, multi-line signatures

### Phase 3: Validation
1. Implement `Issue` ADT
2. Implement comparison logic in `ScaladocChecker`
3. Generate human-readable reports

### Phase 4: Fixing
1. Implement `Fixer` with TODO insertion
2. Implement indentation normalization (only for modified blocks)
3. Handle code block preservation
4. Write modified files back

### Phase 5: CLI and Integration
1. Wire up CLI arguments
2. Add JSON output option
3. Summary statistics

## Test Plan

### Unit Tests

#### `ScaladocBlockSpec`
Test Scaladoc parsing and tag extraction:
```scala
"ScaladocBlock" should "extract @param tags" in {
  val block = "/** @param x the x value\n *  @param y the y value\n */"
  // verify params = List("x", "y")
}

"ScaladocBlock" should "extract @tparam tags" in { ... }
"ScaladocBlock" should "extract @return tag" in { ... }
"ScaladocBlock" should "handle empty scaladoc" in { ... }
"ScaladocBlock" should "handle scaladoc with no tags" in { ... }
"ScaladocBlock" should "handle single-line scaladoc" in { ... }
"ScaladocBlock" should "detect one-liner (single line of descriptive content)" in { ... }
"ScaladocBlock" should "detect one-liner even with @param tags present" in { ... }
"ScaladocBlock" should "not consider multi-line descriptive content as one-liner" in { ... }
```

#### `DeclarationSpec`
Test declaration parsing:
```scala
"Declaration" should "parse simple def" in {
  val chunk = "def foo(x: Int): String = ???"
  // verify kind=def, name=foo, params=List("x"), return=Some("String")
}

"Declaration" should "parse def with type params" in {
  val chunk = "def foo[A, B](x: A): B = ???"
  // verify tparams=List("A", "B")
}

"Declaration" should "parse class with params" in {
  val chunk = "class Foo[T](val x: Int, y: String)"
  // verify kind=class, tparams=List("T"), params=List("x", "y")
}

"Declaration" should "parse def with multiple param lists" in {
  val chunk = "def foo(x: Int)(y: String): Unit = ???"
  // verify params=List("x", "y")
}

"Declaration" should "handle implicit/using params" in {
  val chunk = "def foo(x: Int)(using ctx: Context): Unit"
  // verify params include "x" and "ctx"
}

"Declaration" should "parse trait" in { ... }
"Declaration" should "parse object" in { ... }
"Declaration" should "handle annotations" in { ... }
"Declaration" should "handle modifiers" in { ... }
```

#### `ScaladocCheckerSpec`
Test validation logic:
```scala
"ScaladocChecker" should "detect missing @param" in { ... }
"ScaladocChecker" should "detect extra @param" in { ... }
"ScaladocChecker" should "detect missing @tparam" in { ... }
"ScaladocChecker" should "detect missing @return for non-Unit" in { ... }
"ScaladocChecker" should "detect unnecessary @return for Unit" in { ... }
"ScaladocChecker" should "pass when all tags present" in { ... }
"ScaladocChecker" should "ignore @return for class/trait" in { ... }
```

#### `FixerSpec`
Test fix generation and indentation:
```scala
"Fixer" should "insert missing @param tag" in { ... }
"Fixer" should "insert missing @tparam tag" in { ... }
"Fixer" should "insert missing @return tag" in { ... }
"Fixer" should "NOT insert @return for one-liner scaladoc" in { ... }
"Fixer" should "NOT insert @return for one-liner with @param tags" in { ... }
"Fixer" should "insert @return for multi-line descriptive content" in { ... }
"Fixer" should "preserve existing tags when inserting" in { ... }
"Fixer" should "normalize indentation when fixing" in { ... }
"Fixer" should "preserve single-line scaladoc when no fix needed" in { ... }
"Fixer" should "convert single-line to multi-line when adding tags" in { ... }
"Fixer" should "preserve code blocks" in { ... }
"Fixer" should "align * with first * of /**" in { ... }
```

### Integration Tests

#### `IntegrationSpec`
End-to-end tests with sample Scala files:
```scala
"TodoWriter" should "process a file with no issues" in {
  // Create temp file with correct scaladoc
  // Run checker, verify no issues
}

"TodoWriter" should "detect and fix missing params" in {
  // Create temp file with missing @param
  // Run checker with fix
  // Verify TODO inserted with correct indentation
}

"TodoWriter" should "handle real-world scala code patterns" in {
  // Test with various declaration styles
}

"TodoWriter" should "preserve file when no changes needed" in {
  // Verify file not modified if no issues
}
```

### Test Data
Create test fixtures in `src/test/resources/`:
- `valid.scala` - file with complete documentation
- `missing-params.scala` - file with missing @param tags
- `missing-tparams.scala` - file with missing @tparam tags
- `missing-return.scala` - file with missing @return
- `one-liners.scala` - file with one-liner scaladocs (should NOT get @return added)
- `mixed-issues.scala` - file with various issues
- `edge-cases.scala` - annotations, modifiers, complex signatures

## Dependencies

```scala
// build.sbt
scalaVersion := "3.3.3"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.18" % Test
)
```

## CLI Interface

```
todo-writer - Scaladoc tag checker and fixer

Usage: todo-writer <folder> [options]

Arguments:
  folder              Root folder to scan for .scala files

Options:
  --fix               Insert TODO placeholders for missing tags
  --json              Output results as JSON
  --help              Show this help message

Exit codes:
  0                   No issues found (or --fix applied successfully)
  1                   Issues found (dry run)
  2                   Error (folder not found, etc.)
```

## Example Output

### Dry Run (no --fix)
```
File: src/main/scala/Example.scala
  At line 15: def processData
    - Missing @param for: input, config
    - Missing @return for non-Unit return type

File: src/main/scala/Utils.scala
  At line 42: class Handler[T]
    - Missing @tparam for: T
    - Missing @param for: callback

Summary:
  Methods: 45, with issues: 12 (26.7%)
  Classes: 8, with issues: 2 (25.0%)
  Traits: 3, with issues: 0 (0.0%)

Scanned 15 files. Found 18 issues.
```

### With --fix
Same output, plus:
```
Applied fixes to src/main/scala/Example.scala
Applied fixes to src/main/scala/Utils.scala
```

## Notes

- The tool uses heuristic regex-based parsing, similar to the Python implementation
- It does not use the Scala compiler's parser (keeps dependencies minimal)
- Edge cases in complex declarations may not be handled perfectly
- The indentation normalization only applies to blocks being modified (not all blocks)
