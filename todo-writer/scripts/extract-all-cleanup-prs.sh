#!/usr/bin/env bash
#
# extract-all-cleanup-prs.sh
#
# Run extract-cleanup-pr.sh nine times to produce all nine cleanup PR
# branches in one go. Each branch is created off main with a single
# squashed commit summarizing that partition's @param/@tparam/@return
# additions.
#
# Bails on first failure. If a partial run leaves some branches in place,
# `git branch -D <branch>` them before re-running.
#
# Environment overrides (passed through to extract-cleanup-pr.sh):
#   SOURCE_BRANCH  default: feature-fill-in-cleanup-param-tparam-return-tags
#   BASE_BRANCH    default: main
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EXTRACT="$SCRIPT_DIR/extract-cleanup-pr.sh"
SOURCE_BRANCH=${SOURCE_BRANCH:-"feature-fill-in-cleanup-param-tparam-return-tags"}

if [ ! -x "$EXTRACT" ]; then
  echo "Error: $EXTRACT not found or not executable." >&2
  exit 1
fi

cd "$(git rev-parse --show-toplevel)"

# extract-cleanup-pr.sh leaves us on the newly created cleanup branch
# (which is off main and does not include todo-writer/). To run the next
# invocation we have to switch back to the source branch where
# todo-writer/scripts/ exists in the working tree.
ensure_source() {
  local current
  current=$(git branch --show-current)
  if [ "$current" != "$SOURCE_BRANCH" ]; then
    git checkout --quiet "$SOURCE_BRANCH"
  fi
}

# Run extract-cleanup-pr.sh, but skip if the target branch already exists
# (so this script is restartable after a partial run).
try_extract() {
  local branch=$1
  if git rev-parse --verify --quiet "$branch" >/dev/null; then
    echo "  '$branch' already exists; skipping. (Delete with 'git branch -D $branch' to redo.)"
    return 0
  fi
  "$EXTRACT" "$@"
}

step() {
  echo
  echo "================================================================"
  echo "$1"
  echo "================================================================"
  ensure_source
}

step "1/9  scaladoc-tags-sys-concurrent-runtime-cleanup"
try_extract \
  scaladoc-tags-sys-concurrent-runtime-cleanup \
  "scala.sys, scala.concurrent, scala.runtime" \
  library/src/scala/sys library/src/scala/concurrent library/src/scala/runtime

step "2/9  scaladoc-tags-collection-mutable-cleanup"
try_extract \
  scaladoc-tags-collection-mutable-cleanup \
  "scala.collection.mutable" \
  library/src/scala/collection/mutable library-js/src/scala/collection/mutable

step "3/9  scaladoc-tags-collection-immutable-cleanup"
try_extract \
  scaladoc-tags-collection-immutable-cleanup \
  "scala.collection.immutable" \
  library/src/scala/collection/immutable library-js/src/scala/collection/immutable

step "4/9  scaladoc-tags-collection-core-cleanup"
try_extract \
  scaladoc-tags-collection-core-cleanup \
  "scala.collection (root, convert, generic)" \
  library/src/scala/collection/*.scala \
  library/src/scala/collection/convert \
  library/src/scala/collection/generic

step "5/9  scaladoc-tags-math-cleanup"
try_extract \
  scaladoc-tags-math-cleanup \
  "scala.math" \
  library/src/scala/math

step "6/9  scaladoc-tags-quoted-compiletime-cleanup"
try_extract \
  scaladoc-tags-quoted-compiletime-cleanup \
  "scala.quoted and scala.compiletime" \
  library/src/scala/quoted library/src/scala/compiletime

step "7/9  scaladoc-tags-jdk-cleanup"
try_extract \
  scaladoc-tags-jdk-cleanup \
  "scala.jdk" \
  library/src/scala/jdk

step "8/9  scaladoc-tags-util-io-ref-cleanup"
try_extract \
  scaladoc-tags-util-io-ref-cleanup \
  "scala.util, scala.io, scala.ref" \
  library/src/scala/util library/src/scala/io library/src/scala/ref \
  library-js/src/scala/util

step "9/9  scaladoc-tags-root-files-cleanup"
try_extract \
  scaladoc-tags-root-files-cleanup \
  "loose root files (Array, IArray, Option, Predef, Function, Tuple, MatchError, and numeric types)" \
  library/src/scala/*.scala \
  library-js/src/scala/*.scala

echo
echo "================================================================"
echo "All 9 cleanup PR branches created. Summary:"
echo "================================================================"
for b in \
  scaladoc-tags-sys-concurrent-runtime-cleanup \
  scaladoc-tags-collection-mutable-cleanup \
  scaladoc-tags-collection-immutable-cleanup \
  scaladoc-tags-collection-core-cleanup \
  scaladoc-tags-math-cleanup \
  scaladoc-tags-quoted-compiletime-cleanup \
  scaladoc-tags-jdk-cleanup \
  scaladoc-tags-util-io-ref-cleanup \
  scaladoc-tags-root-files-cleanup; do
  stats=$(git diff --stat "main..$b" 2>/dev/null | tail -1)
  printf "  %-58s  %s\n" "$b" "$stats"
done
