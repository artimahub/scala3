#!/bin/bash

# =============================================================================
# fill-todos-loop.sh
#
# Automated workflow to fill in TODO placeholders in Scaladoc comments.
# Cherry-picks commits from source branch, uses AI to fill in TODOs,
# gets AI review, refines, and commits.
# =============================================================================

set -e

# Configuration (can be overridden via environment variables)
PAUSE_SECONDS=${PAUSE_SECONDS:-60}
MAX_FILES=${MAX_FILES:-10}
SOURCE_BRANCH=${SOURCE_BRANCH:-"feature-create-cleanup-param-tparam-return-todos"}
COMMIT_MSG_PATTERN="Todo-writer added TODOs for @param, @tparam, and @return tags."
DRY_RUN=${DRY_RUN:-false}

# Directories. The script and its prompts live in todo-writer/scripts/;
# reviews and the log file live one level up in todo-writer/.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TODO_WRITER_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
REPO_ROOT=$(git rev-parse --show-toplevel)
REVIEWS_DIR="$TODO_WRITER_DIR/reviews"
LOG_FILE="$TODO_WRITER_DIR/fill-todos.log"
PROMPTS_DIR="$SCRIPT_DIR/prompts"

# Create reviews directory if it doesn't exist (prompts must already exist).
mkdir -p "$REVIEWS_DIR"

# =============================================================================
# Logging
# =============================================================================

log() {
    local msg="[$(date '+%Y-%m-%d %H:%M:%S')] $1"
    echo "$msg"
    echo "$msg" >> "$LOG_FILE"
}

log_error() {
    local msg="[$(date '+%Y-%m-%d %H:%M:%S')] ERROR: $1"
    echo "$msg" >&2
    echo "$msg" >> "$LOG_FILE"
}

# =============================================================================
# Main Script
# =============================================================================

cd "$REPO_ROOT"

# Use current branch as target (cherry-picks will be applied here)
CURRENT_BRANCH=$(git branch --show-current)

log "=============================================="
log "Starting fill-todos-loop.sh"
log "Source branch: $SOURCE_BRANCH"
log "Target branch: $CURRENT_BRANCH (current)"
log "Max files: $MAX_FILES"
log "Pause between files: $PAUSE_SECONDS seconds"
log "Dry run: $DRY_RUN"
log "=============================================="

# Safety check: don't run on the source branch
if [ "$CURRENT_BRANCH" = "$SOURCE_BRANCH" ]; then
    log_error "Cannot run on source branch ($SOURCE_BRANCH). Please checkout a different branch first."
    exit 1
fi

# Get list of commits from source branch with the specific message
# --reverse gives us oldest first, so we process in order
log "Finding commits to process..."
COMMITS=$(git log "$CURRENT_BRANCH".."$SOURCE_BRANCH" --oneline --grep="$COMMIT_MSG_PATTERN" --reverse --format="%H" 2>/dev/null || echo "")

if [ -z "$COMMITS" ]; then
    log "No commits found to process. Either all commits have been cherry-picked, or the source branch doesn't exist."
    exit 0
fi

TOTAL_COMMITS=$(echo "$COMMITS" | wc -l | tr -d ' ')
log "Found $TOTAL_COMMITS commits to process (will process up to $MAX_FILES)"

PROCESSED=0
SKIPPED=0
FAILED=0

for COMMIT in $COMMITS; do
    # Check if we've hit the max
    if [ $PROCESSED -ge $MAX_FILES ]; then
        log "Reached max files limit ($MAX_FILES). Stopping."
        break
    fi

    log ""
    log "=============================================="
    log "Processing commit $((PROCESSED + 1))/$MAX_FILES: $COMMIT"
    log "=============================================="

    # Get commit info
    COMMIT_SHORT=$(git rev-parse --short "$COMMIT")
    FILE_IN_COMMIT=$(git diff-tree --no-commit-id --name-only -r "$COMMIT" | head -1)
    log "File in commit: $FILE_IN_COMMIT"

    # Create a safe filename for storing review
    SAFE_FILENAME=$(echo "$FILE_IN_COMMIT" | tr '/' '_')
    REVIEW_FILE="$REVIEWS_DIR/${SAFE_FILENAME}.json"

    # 1. Cherry-pick the commit
    log "Step 1: Cherry-picking commit..."
    if ! git cherry-pick "$COMMIT" 2>/dev/null; then
        log_error "Cherry-pick failed for $COMMIT"
        git cherry-pick --abort 2>/dev/null || true
        FAILED=$((FAILED + 1))
        continue
    fi
    log "Cherry-pick successful"

    if [ "$DRY_RUN" = "true" ]; then
        log "DRY RUN: Would process $FILE_IN_COMMIT"
        git reset --hard HEAD~1
        PROCESSED=$((PROCESSED + 1))
        continue
    fi

    # 2. Run AI #1 (Writer) to fill in TODOs
    log "Step 2: Running Writer AI to fill in TODOs..."
    WRITER_PROMPT=$(cat "$PROMPTS_DIR/writer-prompt.txt" | sed "s|{FILE_PATH}|$REPO_ROOT/$FILE_IN_COMMIT|g")

    claude --dangerously-skip-permissions -p "$WRITER_PROMPT" \
        --allowedTools Edit,Read,Glob,Grep \
        > "$REVIEWS_DIR/${SAFE_FILENAME}_writer.log" 2>&1

    if [ $? -ne 0 ]; then
        log_error "Writer AI failed for $FILE_IN_COMMIT"
        cat "$REVIEWS_DIR/${SAFE_FILENAME}_writer.log" >> "$LOG_FILE"
        FAILED=$((FAILED + 1))
        # Reset and continue
        git checkout HEAD -- "$FILE_IN_COMMIT"
        git reset --hard HEAD~1
        continue
    fi
    log "Writer AI completed"

    # 3. Run AI #2 (Reviewer) to check the work
    log "Step 3: Running Reviewer AI..."
    REVIEWER_PROMPT=$(cat "$PROMPTS_DIR/reviewer-prompt.txt" | sed "s|{FILE_PATH}|$REPO_ROOT/$FILE_IN_COMMIT|g")

    # Capture JSON output
    REVIEW_OUTPUT=$(claude --dangerously-skip-permissions -p "$REVIEWER_PROMPT" \
        --allowedTools Read,Bash,Grep \
        --output-format json 2>/dev/null)

    # Extract the result text from JSON output and save
    echo "$REVIEW_OUTPUT" | jq -r '.result // .content // .' > "$REVIEW_FILE" 2>/dev/null || echo "$REVIEW_OUTPUT" > "$REVIEW_FILE"
    log "Reviewer feedback saved to: $REVIEW_FILE"

    # Check if changes are required
    REQUIRES_CHANGES=$(echo "$REVIEW_OUTPUT" | jq -r '.result' 2>/dev/null | jq -r '.requires_changes // false' 2>/dev/null || echo "false")
    OVERALL_QUALITY=$(echo "$REVIEW_OUTPUT" | jq -r '.result' 2>/dev/null | jq -r '.overall_quality // "unknown"' 2>/dev/null || echo "unknown")
    log "Review quality: $OVERALL_QUALITY, requires_changes: $REQUIRES_CHANGES"

    # 4. Run AI #1 again to incorporate feedback (always do this for consistency)
    log "Step 4: Running Writer AI to incorporate feedback..."
    REFINEMENT_PROMPT=$(cat "$PROMPTS_DIR/refinement-prompt.txt" | sed "s|{FILE_PATH}|$REPO_ROOT/$FILE_IN_COMMIT|g")
    # Inject the review JSON
    REVIEW_JSON=$(cat "$REVIEW_FILE")
    REFINEMENT_PROMPT="$REFINEMENT_PROMPT

REVIEWER FEEDBACK:
$REVIEW_JSON"

    claude --dangerously-skip-permissions -p "$REFINEMENT_PROMPT" \
        --allowedTools Edit,Read \
        > "$REVIEWS_DIR/${SAFE_FILENAME}_refinement.log" 2>&1

    if [ $? -ne 0 ]; then
        log_error "Refinement AI failed for $FILE_IN_COMMIT (continuing with original)"
        cat "$REVIEWS_DIR/${SAFE_FILENAME}_refinement.log" >> "$LOG_FILE"
    else
        log "Refinement completed"
    fi

    # 5. Commit changes
    log "Step 5: Committing changes..."
    git add "$FILE_IN_COMMIT"

    # Amend the cherry-picked commit with the filled-in documentation
    FILL_COMMIT_MSG="Filled in @param, @tparam, and @return documentation.

Original TODO commit: $COMMIT_SHORT
Reviewer quality rating: $OVERALL_QUALITY

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>"

    git commit --amend -m "$FILL_COMMIT_MSG"
    log "Committed successfully"

    PROCESSED=$((PROCESSED + 1))

    # Log summary for this file
    log "Summary: $FILE_IN_COMMIT - Quality: $OVERALL_QUALITY"

    # 6. Pause before next iteration (unless this is the last one)
    REMAINING=$((MAX_FILES - PROCESSED))
    COMMITS_LEFT=$(echo "$COMMITS" | tail -n +$((PROCESSED + 1)) | wc -l | tr -d ' ')

    if [ $REMAINING -gt 0 ] && [ $COMMITS_LEFT -gt 0 ]; then
        log "Pausing for $PAUSE_SECONDS seconds before next file..."
        sleep "$PAUSE_SECONDS"
    fi
done

log ""
log "=============================================="
log "COMPLETED"
log "Processed: $PROCESSED"
log "Skipped: $SKIPPED"
log "Failed: $FAILED"
log "Remaining commits in source: $(echo "$COMMITS" | wc -l | tr -d ' ') - $PROCESSED"
log "=============================================="
log ""
log "To continue processing more files, run:"
log "  MAX_FILES=50 ./fill-todos-loop.sh"
log ""
log "Review feedback is stored in: $REVIEWS_DIR"
