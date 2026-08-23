#!/usr/bin/env bash
#
# Cleanup-batch todo-writer run for the 10 merged "@param/@tparam/@return"
# partitions (PRs #1-#10 and #12). Skips PR #11 (annotation, reflect, misc,
# #25381), which is still open.
#
# Mirrors the cleanup-batch run order documented in
# todo-writer/docs/pr-directory-breakdown.md, with PR #11's subdirectories
# (annotation, reflect, caps, deriving) omitted.
#
# Flags:
#   --skip-undocumented   only touch declarations that already have a Scaladoc
#                         block; do not add stub Scaladoc to undocumented ones
#   (no --skip-todo)      so TODO: placeholders are written for missing tags
#   (no --dry)            apply changes
#
# sbt runs in todo-writer/ so the ../library/... paths resolve.

set -euo pipefail

cd "$(dirname "$0")/.."

run() {
  echo
  echo "=== $2 ==="
  sbt "run $1 --skip-undocumented"
}

run "../library/src/scala/collection/mutable"   "PR #2  collection/mutable"
run "../library/src/scala/collection/immutable" "PR #3  collection/immutable"
run "../library/src/scala/math"                 "PR #7  math"
run "../library/src/scala/jdk"                  "PR #9  jdk"
run "../library/src/scala/quoted"               "PR #8  quoted (part 1)"
run "../library/src/scala/compiletime"          "PR #8  compiletime (part 2)"
run "../library/src/scala/sys"                  "PR #1  sys (part 1)"
run "../library/src/scala/concurrent"           "PR #1  concurrent (part 2)"
run "../library/src/scala/runtime"              "PR #1  runtime (part 3) — also covers PR #12 AbstractFunction*.scala"
run "../library/src/scala/util"                 "PR #10 util (part 1)"
run "../library/src/scala/io"                   "PR #10 io (part 2)"
run "../library/src/scala/ref"                  "PR #10 ref (part 3)"
run "../library/src/scala/collection"           "PR #4  collection core — revert mutable/, immutable/ before committing"

# Root-level files for PRs #5, #6, #12. This recurses into the whole
# library/src/scala tree, which includes PR #11's directories
# (annotation, reflect, caps, deriving) and PR #11's root files
# (CanEqual, Console, Conversion, Enumeration, Equals, NamedTuple,
# NotImplementedError, PartialFunction, ScalaReflectionException,
# StringContext, Symbol, UninitializedFieldError, specialized, throws,
# typeConstraints). Revert those before committing the root-files PRs.
run "../library/src/scala" "PRs #5, #6, #12 root files — revert PR #11's dirs and root files before committing"

# Parallel trees: library-js mirrors much of library/'s structure (concurrent,
# runtime, util, collection, math, reflect, plus root files); library-aux has
# only a handful of root files. PR #11 also touches a few library-js files
# (Console, Enumeration, reflect/ClassTag, reflect/Manifest) -- those are
# covered by revert-pr11-files.sh.
run "../library-js/src/scala"  "library-js (whole tree)"
run "../library-aux/src/scala" "library-aux (root files only)"

echo
echo "Done. Reminder: PR #11 (annotation/reflect/misc, #25381) is still open."
echo "Run scripts/revert-pr11-files.sh to undo touches to any file in PR #11,"
echo "across library/, library-js/, and library-aux/, before committing."
