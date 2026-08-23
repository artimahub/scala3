#!/usr/bin/env bash
#
# commit-each-file.sh [path]
#
# For every file currently modified in the working tree (relative to HEAD)
# under the given path, make one commit per file with the standard
# todo-writer commit message. This produces the one-file-per-commit shape
# that fill-todos-loop.sh expects from its source branch.
#
# Default path: library/
#
set -euo pipefail

COMMIT_MSG="Todo-writer added TODOs for @param, @tparam, and @return tags."
PATH_ARG=${1:-library/}

cd "$(git rev-parse --show-toplevel)"

FILES=$(git diff --name-only HEAD -- "$PATH_ARG")

if [ -z "$FILES" ]; then
    echo "No modified files found under $PATH_ARG"
    exit 0
fi

TOTAL=$(echo "$FILES" | wc -l | tr -d ' ')
echo "Found $TOTAL modified files under $PATH_ARG. Starting commits..."
echo "==================="

COUNT=0
while IFS= read -r file; do
    [ -z "$file" ] && continue
    COUNT=$((COUNT + 1))
    echo "[$COUNT/$TOTAL] Committing: $file"
    git add "$file"
    git commit -m "$COMMIT_MSG" --quiet
done <<< "$FILES"

echo "==================="
echo "All $TOTAL commits complete!"
