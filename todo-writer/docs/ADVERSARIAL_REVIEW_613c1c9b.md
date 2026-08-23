# Adversarial review: `613c1c9b320f861fd4034a086bb2c502b5fc5c5a`

Scope: the commit was compared with `main`. The review intentionally makes no
source changes.

## Findings

### P2 — `Future.never` documents transformations that can never occur

`Future.never` returns itself for every transformation, without invoking a
callback or observing the other future.  The new documentation for the legacy
two-function `transform` overload says that it transforms the success/failure
value and returns a new future with that result.  The comments for `flatten`,
`filter`, `collect`, `zipWith`, and `mapTo` make the same claim.  This conflicts
with their implementations: each is simply `= this` and the receiver never
completes.

This is particularly misleading for `zipWith`: users may expect completion
when `that` completes, whereas the returned future remains pending forever.
Document these overrides consistently with the neighbouring `map` and
`flatMap` comments: no supplied function is invoked, no new future is created,
and the same never-completing future is returned.

Location: `library/src/scala/concurrent/Future.scala:699-824`.

### P2 — Two adjacent Scaladoc comments leave `AbstractBatch.other` undocumented

The two comments at the beginning of `AbstractBatch`'s parameter list are both
placed immediately before `first`.  Consequently, the comment intended for
`other` is not adjacent to that parameter, and the comment intended for
`first` is a separate, unattached Scaladoc comment.  The generated API cannot
reliably associate the descriptions with the intended constructor fields.

Place each parameter description immediately before its own declaration (or
describe them in the class documentation using `@param`).

Location: `library/src/scala/concurrent/BatchingExecutor.scala:115-121`.

## Validation notes

- `git diff --check main...613c1c9b320f861fd4034a086bb2c502b5fc5c5a` completed without whitespace errors.
- I started `sbt --client 'scala-library-nonbootstrapped/doc'` to validate
  Scaladoc. It was still compiling the library when this review was written, so
  this report does not claim that task passed.
