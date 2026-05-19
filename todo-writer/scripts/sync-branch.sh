#!/bin/bash

set -euo pipefail

# Check if branch name was provided
if [ $# -ne 1 ]; then
    echo "Usage: $0 <branch-name>"
    exit 1
fi

BRANCH="$1"
FORK_REMOTE="${FORK_REMOTE:-origin}"  # Default to 'origin', override with env var

echo "Syncing branch '$BRANCH' with main..."

# Make sure we have latest main
git fetch origin main

# Switch to the target branch and get the commit to cherry-pick
git checkout "$BRANCH"
COMMIT_HASH=$(git rev-parse HEAD)
echo "Commit to cherry-pick: $COMMIT_HASH"

# Reset the branch to main
git reset --hard origin/main
echo "Reset '$BRANCH' to origin/main"

# Cherry-pick the commit
git cherry-pick "$COMMIT_HASH"
echo "Cherry-picked commit $COMMIT_HASH"

# Print the push command for manual execution
echo ""
echo "Review the changes, then run:"
echo "  git push --force-with-lease $FORK_REMOTE $BRANCH"
