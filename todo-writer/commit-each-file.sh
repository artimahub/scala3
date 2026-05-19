#!/bin/bash

COMMIT_MSG="Todo-writer added TODOs for @param, @tparam, and @return tags."

# Get the repo root
REPO_ROOT=$(git rev-parse --show-toplevel)
echo "Repo root: $REPO_ROOT"

# Change to repo root so paths work correctly
cd "$REPO_ROOT"
echo "Changed to: $(pwd)"

# Get all modified files under library/
echo "Finding modified files under library-js/..."
FILES=$(git diff --name-only HEAD -- library-js/)

if [ -z "$FILES" ]; then
    echo "No modified files found under library-js/"
    exit 0
fi

echo "Found files:"
echo "$FILES"
echo ""
echo "Starting commits..."
echo "==================="

COUNT=0
echo "$FILES" | while read -r file; do
    if [ -n "$file" ]; then
        COUNT=$((COUNT + 1))
        echo ""
        echo "[$COUNT] Committing: $file"
        git add "$file"
        git commit -m "$COMMIT_MSG"
        echo "    Done."
    fi
done

echo ""
echo "==================="
echo "All commits complete!"
