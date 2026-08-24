# Codex branch reviews, weeks 3-12

One review per open pull request, produced by `todo-writer/scripts/review-branches.sh`
against `upstream/main` at `190abc3480` with `gpt-5.6-terra`, on 2026-08-24.

Each branch was reviewed in full: the whole branch against main, not just its
most recent commit. The diffs (166KB to 453KB each) were split into chunks of
whole files of roughly 60KB of diff, reviewed separately and merged back per
branch. 65 calls in total, all successful, none retried.

## Files

- `INDEX.md` - verdict and counts per branch.
- `wkNN-<branch>.review.json` - the merged review for one pull request.
- `wkNN-<branch>.chunkK.json` - the chunk results it was merged from.
- `chunk-logs.tar.gz` - the 65 Codex transcripts, 19MB unpacked.

Unpack the transcripts with `tar xzf chunk-logs.tar.gz`. They are worth keeping
for one reason: they show how much source each review actually read. An
`approve` with no findings means something different depending on whether the
reviewer opened the files. For example `jdk/FunctionWrappers.scala` (365
near-identical wrappers) got 2 file reads in 43 seconds, while
`quoted/Quotes.scala` got 16 reads and 53 ripgreps over 3 minutes. Both
verdicts are probably right; only one of them was earned by looking.

## Shape of a review

```
verdict           "approve" only if nothing is a blocker; otherwise "revise"
blockers          items with severity "blocker"
nits              items with severity "nit"
needs_human       items the code itself cannot settle
items[]           file, symbol, line, severity, issue, suggestion,
                  confidence, needs_human
bonus_findings[]  problems in PRE-EXISTING documentation or code, outside the
                  added comments; never affect the verdict
```

## Totals

245 blockers, 54 nits, 24 needs-human, 17 bonus, across 339 assessed items.
Every branch came back "revise".

By kind: 172 misstatements, 58 undocumented throwables, 15 NPE-on-null.
Two thirds are misstatements, the category the brief calls worse than leaving
documentation blank.

## Read these as raw input, not as conclusions

Nothing here has been adjudicated. Some findings will be wrong. Three
cross-cutting questions should be settled before individual items are worth
arguing about:

1. **NPE-on-null.** 15 blockers say a method does not document that a null
   argument throws. Accept that standard and most reference-taking methods in
   the library need a tag. Probably scope it to methods whose contract is
   actually about null.
2. **One fact or many tags.** `IllegalArgumentException` from an overfull
   `Range` is reached through nine different methods and reported nine times.
   That is one decision, not nine.
3. **Doc comments on `extension` clauses.** `Quotes.scala` `asTerm` and
   `asExprOf` each have a pre-existing comment on the extension and a new one
   on the method. Whether Scaladoc drops one needs checking, then sweeping.

Also note the reviewer independently rediscovered several entries already in
`todo-writer/docs/suspected-bugs.md` (`BoxesRunTime.boxToByte`, the
`StreamExtensions` accumulator factories, `BitSetStepper.semiclone`,
`ArrayBuilder.ofUnit.addAll`) without being shown that file.
