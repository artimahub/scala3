# Experiment outputs

Generated files from writer-model trials, kept for side-by-side comparison.

**Why they live here and not in a scratch directory.** `/tmp` and `/home/node`
are container overlay storage and are destroyed by a devcontainer rebuild. An
earlier set of these outputs was lost that way. `/workspace` is a bind mount to
the Mac, so anything under the repo survives. See `docs/aider-notes.md` section 3
for the three storage classes.

Naming: `<file>.<provider>-<model>[-<note>].scala`
