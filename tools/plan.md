# Scaladoc Checker — Plan

Goal
- Create a Scala 3 CLI tool (script) in `tools/` that finds public or protected declarations missing Scaladoc and inserts a placeholder:
  ```
  /** TODO FILL IN */
  ```
  above any object, class, trait, val, var, or def that is public or protected and has no Scaladoc.

Scope
- Operate on .scala files under a given root (default: repository root).
- Skip excluded paths (e.g. generated code).
- Support dry-run (report only) and apply (write changes).

Detection heuristics
- Declaration types: class, object, trait, def, val, var.
- Consider a declaration "target" when:
  - Not explicitly private (modifier `private` present).
  - Explicitly `protected` or top-level / member-level public.
  - For vals/vars and defs, avoid local (inside method) declarations by using indentation/scan heuristics.
  - Treat secondary constructors (`def this(...)`) as members.
- Scaladoc presence:
  - Look for a `/** ... */` block above the declaration whose closing `*/` is before the declaration.
  - Ensure only blank lines, annotations (`@...`), or comment lines appear between doc end and declaration.
  - Avoid false positives by scanning a reasonable lookback window.
- Idempotency:
  - Do not re-insert if a valid Scaladoc exists.
  - Do not insert duplicate placeholder if one already exists directly above.
  - Attempt limited cleanup of prior placeholder lines that are adjacent to a real Scaladoc.

Safety & I/O
- Default to dry-run mode. Require explicit `--apply` to modify files.
- When applying, write atomically (write a tmp file then move/replace original).
- Encoding: UTF-8.
- Preserve existing file newline semantics where practical.

CLI
- scala tools/scaladoc_checker.scala -- [--dry-run] [--apply] [--root PATH] [--exclude pat1,pat2]

Testing & Validation
- Run dry-run over `library/` and review planned edits; iterate heuristics on false positives/negatives.
- Add a few unit-like sample files under `tools/testdata/` to exercise edge cases:
  - Multi-line Scaladoc with blank line then declaration.
  - Annotations between doc and declaration.
  - Local vs member vals/vars/defs.
  - `def this(...)` constructors.

Implementation notes
- Prefer a line-oriented parser using regex + indentation heuristics (avoid full AST parsing to keep the tool self-contained).
- Keep lookback windows configurable constants.
- Keep the tool as a single-file Scala 3 script for easy invocation.

Deliverables
- `tools/plan.md` (this file)
- `tools/scaladoc_checker.scala` (Scala 3 script implementing the tool)