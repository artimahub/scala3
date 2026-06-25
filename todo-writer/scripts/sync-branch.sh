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

# Switch to the target branch and record its tip. We replay every commit the
# branch has above main, not just HEAD, so branches with more than one commit
# (e.g. a cleanup commit plus a separate redundant-@return removal commit) keep
# all of their commits after the sync.
git checkout "$BRANCH"
ORIG_TIP=$(git rev-parse HEAD)
echo "Branch tip to replay onto main: $ORIG_TIP"
echo "Commits to cherry-pick (oldest first):"
git log --oneline --reverse origin/main..$ORIG_TIP | sed 's/^/  /'

# Reset the branch to main
git reset --hard origin/main
echo "Reset '$BRANCH' to origin/main"

# Cherry-pick every commit the branch had above main, oldest first.
git cherry-pick origin/main..$ORIG_TIP
echo "Cherry-picked $(git rev-list --count origin/main..HEAD) commit(s) onto origin/main"

# Print the push command for manual execution
echo ""
echo "Review the changes, then run:"
echo "  git push --force-with-lease $FORK_REMOTE $BRANCH"
