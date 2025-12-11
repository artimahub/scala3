# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is the Scala 3 (Dotty) compiler repository. It includes the compiler, standard library, REPL, and related tooling for the Scala 3 programming language.

## Build System

The project uses sbt. Key commands:

```bash
# Compile the compiler (non-bootstrapped - faster for development)
sbt scala3-compiler-nonbootstrapped/compile

# Compile the compiler (bootstrapped - compiles itself)
sbt scala3-compiler-bootstrapped-new/compile

# Run the REPL
sbt scala3-repl/run

# Build a local distribution
sbt dist/Universal/packageBin
```

## Running Tests

```bash
# Run all compilation tests (pos, neg, run tests)
sbt testCompilation

# Run tests with a filter (substring match on test path)
sbt "testCompilation substring"

# Run tests and update checkfiles when output changes
sbt "testCompilation --update-checkfiles"

# Re-run only failed tests
sbt "testCompilation --failed"

# Run specific test suites directly
sbt "scala3-compiler-bootstrapped-new/testOnly dotty.tools.dotc.CompilationTests"
sbt "scala3-compiler-bootstrapped-new/testOnly dotty.tools.dotc.FromTastyTests"

# Run a single JUnit test class
sbt "scala3-compiler-nonbootstrapped/testOnly dotty.tools.dotc.parsing.DocstringTests"
```

## Test Directory Structure

Tests are in `tests/` with these key directories:
- `tests/pos/` - Should compile successfully
- `tests/neg/` - Should fail with expected errors (`.check` files contain expected output)
- `tests/run/` - Should compile and run, with expected output in `.check` files
- `tests/warn/` - Should compile with expected warnings

Tests can be single `.scala` files or directories containing multiple files that compile together.

## Project Structure

### Compiler (compiler/)

The compiler is in `compiler/src/dotty/tools/dotc/`:
- `ast/` - Tree definitions and operations
- `core/` - Core data structures: Types, Symbols, Contexts, Denotations
- `parsing/` - Scanner and Parser
- `typer/` - Type checking and inference
- `transform/` - Tree transformation phases (post-typer to pre-backend)
- `backend/jvm/` - JVM bytecode generation
- `reporting/` - Error and warning messages

Key files:
- `Compiler.scala` - Defines compiler phases and their ordering
- `Run.scala` - Represents a single compilation run
- `Driver.scala` - Entry point for running the compiler

### Compiler Phases

Phases are organized in groups (see `Compiler.scala`):
1. **Frontend**: Parser → Typer → PostTyper
2. **Pickler**: Generates TASTY format
3. **Transform**: Many phases that lower the AST (Erasure being a key one)
4. **Backend**: JVM bytecode generation (GenBCode)

### Standard Library (library/)

The Scala 3 standard library extends Scala 2.13's library. Key additions in `library/src/scala/`:
- Tuple operations, IArray, NamedTuple
- Quoted expressions and type class derivation support

### Test Framework (Vulpix)

The custom test framework is in `compiler/test/dotty/tools/vulpix/`. Tests extend `ParallelTesting` and use:
- `compileFile()` / `compileFilesInDir()` / `compileDir()` to define what to compile
- `.checkCompile()` / `.checkRun()` / `.checkRewrites()` to verify results

## Bootstrapping

The compiler has two configurations:
- **Non-bootstrapped**: Built with the reference (published) Scala 3 compiler - faster iteration
- **Bootstrapped**: The compiler compiles itself - required for testing changes to code that affects compilation

Use non-bootstrapped for development, bootstrapped for final testing.

## Scaladoc Comments

Use present tense indicative mood (Creates, Returns, Represents) instead of imperative mood (Create, Return) for method documentation.
