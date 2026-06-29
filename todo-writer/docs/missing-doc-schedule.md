# Missing-documentation rollout schedule

The "Undocumented batch": add Scaladoc to declarations that have **no comment at
all** (the earlier @param/@tparam/@return PRs handled already-documented ones).
~8,360 declarations total. Driven by `scripts/run-missing-doc-todos.sh`.

## Strategy

- **One PR per week**, combining small partitions so each PR is ~300-900 new
  declarations — big enough to be worth a review slot, small enough that one
  reviewer can handle it. The four large collection/runtime partitions stay solo.
- **Small and peripheral first, big and core last.** This is deliberate: each PR's
  reviewer feedback is folded into `docs/house-rules.md` before the next run, so
  the giant `collection` PRs at the end inherit every convention learned earlier.
- **Re-run the same PR if early feedback is heavy** — nothing is merged until
  approved, so the pilot can be regenerated under updated rules cheaply.

## Per-PR loop

```
# 1. generate + fill (combine partitions into one PR)
scripts/run-missing-doc-todos.sh <partition> [<partition> ...]
# 2. review the working tree, commit on a branch, open ONE PR
# 3. when feedback arrives, distill it:
#      - prose/style/convention  -> append a bullet to docs/house-rules.md
#      - what-gets-documented     -> a small ScaladocChecker change
# 4. note the house-rules sha in the PR description, then run the next PR
```

Preview any PR's exact files/counts without spending tokens:
`scripts/run-missing-doc-todos.sh --plan <partition> [...]`

## Schedule (~11 weeks, one PR/week)

| Wk | PR (partitions)                              | ~decls | notes |
|----|----------------------------------------------|-------:|-------|
| 1  | `io-ref` `root-numeric`                      |   140  | **pilot** — small & varied; re-run after first feedback |
| 2  | `misc-dirs` `root-misc`                      |   312  | reflect/annotation dirs + loose root files (~ old PR #11) |
| 3  | `root-array-predef` `root-function-tuple` `sys` | 400 | Array/IArray/Option/Predef + Function/Tuple/Product + sys |
| 4  | `util` `concurrent`                          |   480  | |
| 5  | `math` `coll-generic`                        |   554  | first taste of collection (generic + concurrent) |
| 6  | `quoted` `jdk`                               |   863  | |
| 7  | `coll-convert` `library-js`                  |   901  | |
| 8  | `runtime`                                    |   687  | solo (141 small files) |
| 9  | `coll-mutable`                               |  1048  | solo |
| 10 | `coll-core`                                  |  1257  | solo (collection top-level, excludes subtrees) |
| 11 | `coll-immutable`                             |  1739  | solo; split by file if reviewers want it smaller |

To compress to 10 weeks, merge wk1+wk2. If `coll-immutable` is too big for one
review, split it into two file-list PRs (Vector/HashMap/ArraySeq/Map/HashSet are
the heavy files) — that pushes to 12 weeks.

## Cost lever

PR grouping changes the calendar, not the token total. If the token budget is
tight, trim per-file cost on the low-risk early PRs with `MAX_ROUNDS=1` and/or a
cheaper `WRITER_MODEL`/`STYLE_MODEL`; keep full quality (default `MAX_ROUNDS=2`)
for the `collection` PRs.
