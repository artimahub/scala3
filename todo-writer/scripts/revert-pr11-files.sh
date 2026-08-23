#!/usr/bin/env bash
#
# revert-pr11-files.sh
#
# Undo working-tree changes to any file that PR #11
# (scaladoc-tags-annotation-reflect-misc) touches, so the cleanup-batch
# run does not collide with that still-open PR. Run this after
# run-cleanup-todos.sh and before committing per-file.
#
# The file list is read dynamically from the PR #11 branch, so it stays
# accurate even if PR #11 is rebased or amended.
#
# Override the branch name with PR11_BRANCH=...
#
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

PR11_BRANCH=${PR11_BRANCH:-scaladoc-tags-annotation-reflect-misc}

if ! git rev-parse --verify --quiet "$PR11_BRANCH" >/dev/null; then
    echo "Error: branch '$PR11_BRANCH' not found." >&2
    exit 1
fi

# run-cleanup-todos.sh now touches library/, library-js/, and library-aux/,
# so revert PR #11's footprint across all three trees.
FILES=$(git diff --name-only "main...$PR11_BRANCH" | grep -E '^(library|library-js|library-aux)/' || true)

if [ -z "$FILES" ]; then
    echo "No matching files found in $PR11_BRANCH (vs main). Nothing to revert."
    exit 0
fi

COUNT=$(echo "$FILES" | wc -l | tr -d ' ')
echo "Reverting $COUNT files from $PR11_BRANCH in working tree..."
echo "$FILES" | xargs git checkout HEAD --
echo "Done."
