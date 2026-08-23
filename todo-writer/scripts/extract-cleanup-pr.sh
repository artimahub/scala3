#!/usr/bin/env bash
#
# extract-cleanup-pr.sh
#
# For one cleanup-PR partition: create a branch off main, cherry-pick the
# "Filled in @param/@tparam/@return..." commits from the source branch that
# touch the given paths, then squash into a single commit with the standard
# message:
#
#   Add missing @param/@tparam/@return Scaladoc tags in <description> (cleanup follow-up)
#
# Usage:
#   ./extract-cleanup-pr.sh <new-branch-name> "<description>" <path1> [<path2> ...]
#
# Example:
#   ./extract-cleanup-pr.sh scaladoc-tags-collection-mutable-cleanup \
#       "scala.collection.mutable" \
#       library/src/scala/collection/mutable \
#       library-js/src/scala/collection/mutable
#
# Environment overrides:
#   SOURCE_BRANCH  default: feature-fill-in-cleanup-param-tparam-return-tags
#   BASE_BRANCH    default: main
#
set -euo pipefail

SOURCE_BRANCH=${SOURCE_BRANCH:-"feature-fill-in-cleanup-param-tparam-return-tags"}
BASE_BRANCH=${BASE_BRANCH:-"main"}

if [ $# -lt 3 ]; then
  cat >&2 <<USAGE
Usage: $(basename "$0") <new-branch-name> "<description>" <path1> [<path2> ...]

Example:
  $(basename "$0") scaladoc-tags-collection-mutable-cleanup \\
      "scala.collection.mutable" \\
      library/src/scala/collection/mutable \\
      library-js/src/scala/collection/mutable

Environment overrides:
  SOURCE_BRANCH  default: $SOURCE_BRANCH
  BASE_BRANCH    default: $BASE_BRANCH
USAGE
  exit 1
fi

NEW_BRANCH=$1
DESCRIPTION=$2
shift 2
PATHS=("$@")

cd "$(git rev-parse --show-toplevel)"

# --- Sanity checks --------------------------------------------------------

if ! git rev-parse --verify --quiet "$SOURCE_BRANCH" >/dev/null; then
  echo "Error: source branch '$SOURCE_BRANCH' not found." >&2
  exit 1
fi

if ! git rev-parse --verify --quiet "$BASE_BRANCH" >/dev/null; then
  echo "Error: base branch '$BASE_BRANCH' not found." >&2
  exit 1
fi

if git rev-parse --verify --quiet "$NEW_BRANCH" >/dev/null; then
  echo "Error: branch '$NEW_BRANCH' already exists. Delete it first or pick another name." >&2
  exit 1
fi

if [ -n "$(git status --porcelain)" ]; then
  echo "Error: working tree not clean. Commit or stash before running." >&2
  exit 1
fi

# --- Find commits to cherry-pick -----------------------------------------

# Commits reachable from SOURCE_BRANCH but not from BASE_BRANCH whose
# message starts with "Filled in" AND that touch at least one of the given
# paths. --reverse gives chronological order so cherry-pick replays cleanly.
COMMITS=$(git log "$BASE_BRANCH..$SOURCE_BRANCH" --reverse --format='%H' \
              --grep='^Filled in' -- "${PATHS[@]}")

if [ -z "$COMMITS" ]; then
  echo "Error: no 'Filled in' commits found in $SOURCE_BRANCH touching:" >&2
  for p in "${PATHS[@]}"; do
    echo "  $p" >&2
  done
  exit 1
fi

COUNT=$(echo "$COMMITS" | wc -l | tr -d ' ')
echo "Found $COUNT fill-in commit(s) for paths: ${PATHS[*]}"
echo "Base: $BASE_BRANCH   New branch: $NEW_BRANCH"

# --- Build the branch ----------------------------------------------------

git checkout --quiet "$BASE_BRANCH"
git checkout --quiet -b "$NEW_BRANCH"

i=0
while IFS= read -r sha; do
  [ -z "$sha" ] && continue
  i=$((i + 1))
  echo "  [$i/$COUNT] cherry-picking $sha"
  git cherry-pick --quiet "$sha"
done <<< "$COMMITS"

# --- Squash into one commit ----------------------------------------------

git reset --quiet --soft "$BASE_BRANCH"
git commit --quiet -m "Add missing @param/@tparam/@return Scaladoc tags in $DESCRIPTION (cleanup follow-up)"

echo ""
echo "Done."
git log --oneline -1
echo ""
echo "Diff stat:"
git diff --stat "$BASE_BRANCH..$NEW_BRANCH" | tail -1
