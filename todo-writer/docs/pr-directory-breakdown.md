# PR directory breakdown

Splits used for the 12 "fill in missing @param, @tparam, and @return tags" PRs.
Each PR is intended to be small enough for one reviewer to handle.

Two batches are planned per partition:

1. **Cleanup batch (12 PRs)** — re-run with `--skip-undocumented` to pick up
   missing tags on declarations that *already have* a Scaladoc block. Picks up
   anything the older todo-writer missed (HKTs, multi-param-list constructors,
   backticked/qualified `@param` names, stricter `@return` detection, etc.).
2. **Undocumented batch (12 PRs)** — re-run *without* `--skip-undocumented`
   to add stub Scaladoc to declarations that have no comment at all.

## The 12 partitions

| #  | PR (last round)               | Status | Paths to run on                                                                 |
|----|-------------------------------|--------|---------------------------------------------------------------------------------|
| 1  | #25371 Sys, concurrent, runtime | MERGED | `library/src/scala/sys`, `library/src/scala/concurrent`, `library/src/scala/runtime` |
| 2  | #25372 Collection mutable     | MERGED | `library/src/scala/collection/mutable` |
| 3  | #25373 Collection immutable   | MERGED | `library/src/scala/collection/immutable` |
| 4  | #25374 Collection core        | MERGED | `library/src/scala/collection` top-level + `concurrent`, `convert`, `generic` (exclude `mutable`, `immutable`) |
| 5  | #25375 Numeric types          | MERGED | `library/src/scala/{Boolean,Byte,Char,Double,Float,Int,Long,Short}.scala` |
| 6  | #25376 Array, IArray, Predef, Option | MERGED | `library/src/scala/{Array,IArray,Option,Predef}.scala` |
| 7  | #25377 Math                   | MERGED | `library/src/scala/math` |
| 8  | #25378 Quoted, compiletime    | MERGED | `library/src/scala/quoted`, `library/src/scala/compiletime` |
| 9  | #25379 Jdk                    | MERGED | `library/src/scala/jdk` |
| 10 | #25380 Util, io, ref          | MERGED | `library/src/scala/util`, `library/src/scala/io`, `library/src/scala/ref` |
| 11 | #25381 Annotation, reflect, misc | **OPEN** | `library/src/scala/annotation`, `library/src/scala/reflect`, `library/src/scala/caps`, `library/src/scala/deriving` + root files: `CanEqual`, `Console`, `Conversion`, `Enumeration`, `Equals`, `NamedTuple`, `NotImplementedError`, `PartialFunction`, `ScalaReflectionException`, `StringContext`, `Symbol`, `UninitializedFieldError`, `specialized`, `throws`, `typeConstraints` |
| 12 | #25996 Function, Tuple, Product (V2) | **OPEN** | `library/src/scala/{Function,Tuple,Product}*.scala` + `library/src/scala/runtime/AbstractFunction*.scala` |

## Running notes

todo-writer takes a single directory and recurses (`Files.walk`). It does not
accept individual files or globs. That makes the partitions fall into three
groups:

- **Single directory** — points to one dir, run once. (#2, #3, #7, #9)
- **Multiple sibling directories** — run the tool 2–3 times. (#1, #8, #10)
- **Has loose root files** — `library/src/scala/*.scala` doesn't fit cleanly,
  since pointing at `library/src/scala` recurses into everything below it.
  Workaround: run on the broader scope, then `git checkout` the files outside
  the PR's intended file list before committing. (#5, #6, #11, #12)
- **Has excluded children** — #4 collection-core: running on
  `library/src/scala/collection` re-picks up `mutable`/`immutable`. Same
  workaround: run broader, check out the excluded subtrees before committing.

## Cleanup-batch run order

Skip #11 and #12 until those PRs are merged. The other ten can be done in
any order; one directory at a time, one PR per partition.

```
sbt 'run ../library/src/scala/collection/mutable     --skip-undocumented --dry'   # PR #2
sbt 'run ../library/src/scala/collection/immutable   --skip-undocumented --dry'   # PR #3
sbt 'run ../library/src/scala/math                   --skip-undocumented --dry'   # PR #7
sbt 'run ../library/src/scala/jdk                    --skip-undocumented --dry'   # PR #9
sbt 'run ../library/src/scala/quoted                 --skip-undocumented --dry'   # PR #8 (part 1)
sbt 'run ../library/src/scala/compiletime            --skip-undocumented --dry'   # PR #8 (part 2)
sbt 'run ../library/src/scala/sys                    --skip-undocumented --dry'   # PR #1 (part 1)
sbt 'run ../library/src/scala/concurrent             --skip-undocumented --dry'   # PR #1 (part 2)
sbt 'run ../library/src/scala/runtime                --skip-undocumented --dry'   # PR #1 (part 3)
sbt 'run ../library/src/scala/util                   --skip-undocumented --dry'   # PR #10 (part 1)
sbt 'run ../library/src/scala/io                     --skip-undocumented --dry'   # PR #10 (part 2)
sbt 'run ../library/src/scala/ref                    --skip-undocumented --dry'   # PR #10 (part 3)
sbt 'run ../library/src/scala/collection             --skip-undocumented --dry'   # PR #4 — then revert mutable/, immutable/
sbt 'run ../library/src/scala                        --skip-undocumented --dry'   # PRs #5, #6 — revert everything except the listed root files
```

Drop `--dry` and add `--skip-todo` (or omit it) to apply.

For the undocumented batch, drop `--skip-undocumented` so the synthetic
"declaration has no Scaladoc" detection runs.
