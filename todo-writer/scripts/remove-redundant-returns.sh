#!/usr/bin/env bash
# =============================================================================
# remove-redundant-returns.sh
#
# For each "scaladoc-tags-*-cleanup" branch, create a work-branch and remove the
# @return tags that the cleanup commit added but that are CLEARLY redundant with
# the method's main description (per the Scala doc guideline). Each removal is a
# separate commit so they can be reverted individually.
#
# Strategy (per branch X):
#   1. Base a new branch  X-rm-redundant-return  on origin/X (GitHub state).
#   2. For each changed file, find the @return tags ADDED by the cleanup
#      commit(s) (lines blamed to branch-only commits).
#   3. Hand that file + the added-@return list to Claude, which judges each tag
#      CONSERVATIVELY, removes only clear-cut echoes (plus any orphaned blank
#      ` *` line), and commits each removal on its own.
#   4. Strictly VERIFY every new commit is a pure @return removal touching only
#      that file. On the first violation, reset from that commit onward and log
#      the file for manual review.
#
# The script NEVER pushes. At the end it prints the 9 push commands for you.
#
# Config via env vars (all optional):
#   BRANCHES              space-separated branch list (default: the 9 cleanup branches)
#   WORK_SUFFIX           work-branch suffix (default: -rm-redundant-return)
#   MAIN_REF              baseline ref (default: origin/main)
#   CLAUDE_BIN            claude executable (default: claude)
#   MODEL                 model passed to claude (default: claude's default)
#   PAUSE_SECONDS         pause before each Claude call (default: 30)
#   BRANCH_PAUSE_SECONDS  pause between branches (default: 60)
#   MAX_BRANCHES          process at most N branches (default: 0 = all)
#   MAX_FILES_PER_BRANCH  process at most N files per branch (default: 0 = all)
#   SKIP_EXISTING         skip a work-branch that already exists (default: true)
#   DRY_RUN               list added @return tags, do not call Claude (default: false)
# =============================================================================

set -uo pipefail

# ---- configuration ----------------------------------------------------------
DEFAULT_BRANCHES="\
scaladoc-tags-collection-core-cleanup \
scaladoc-tags-collection-immutable-cleanup \
scaladoc-tags-collection-mutable-cleanup \
scaladoc-tags-jdk-cleanup \
scaladoc-tags-math-cleanup \
scaladoc-tags-quoted-compiletime-cleanup \
scaladoc-tags-root-files-cleanup \
scaladoc-tags-sys-concurrent-runtime-cleanup \
scaladoc-tags-util-io-ref-cleanup"

BRANCHES=${BRANCHES:-$DEFAULT_BRANCHES}
WORK_SUFFIX=${WORK_SUFFIX:--rm-redundant-return}
MAIN_REF=${MAIN_REF:-origin/main}
CLAUDE_BIN=${CLAUDE_BIN:-claude}
MODEL=${MODEL:-}
PAUSE_SECONDS=${PAUSE_SECONDS:-30}
BRANCH_PAUSE_SECONDS=${BRANCH_PAUSE_SECONDS:-60}
MAX_BRANCHES=${MAX_BRANCHES:-0}
MAX_FILES_PER_BRANCH=${MAX_FILES_PER_BRANCH:-0}
SKIP_EXISTING=${SKIP_EXISTING:-true}
DRY_RUN=${DRY_RUN:-false}

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TODO_WRITER_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
REPO_ROOT="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
PROMPT_TEMPLATE="$SCRIPT_DIR/prompts/remove-redundant-return-prompt.txt"
LOG_DIR="$TODO_WRITER_DIR/return-cleanup"
LOG_FILE="$LOG_DIR/remove-redundant-returns.log"
MANUAL_REVIEW="$LOG_DIR/manual-review.txt"
SUMMARY="$LOG_DIR/summary.txt"

mkdir -p "$LOG_DIR"
: > "$MANUAL_REVIEW"
: > "$SUMMARY"

log()       { local m="[$(date '+%Y-%m-%d %H:%M:%S')] $1"; echo "$m"; echo "$m" >> "$LOG_FILE"; }
log_error() { local m="[$(date '+%Y-%m-%d %H:%M:%S')] ERROR: $1"; echo "$m" >&2; echo "$m" >> "$LOG_FILE"; }

cd "$REPO_ROOT"

[ -f "$PROMPT_TEMPLATE" ] || { log_error "Prompt template not found: $PROMPT_TEMPLATE"; exit 1; }

# Refuse to run on a dirty tree (we hard-reset during verification).
if [ -n "$(git status --porcelain)" ]; then
    log_error "Working tree is not clean. Commit or stash your changes first."
    exit 1
fi

START_REF="$(git symbolic-ref --quiet --short HEAD || git rev-parse HEAD)"
log "Starting from: $START_REF"
log "Fetching origin..."
git fetch --quiet origin || { log_error "git fetch failed"; exit 1; }

TEMPLATE_CONTENT="$(cat "$PROMPT_TEMPLATE")"

# Return the @return tags added by branch-only commits in $1 (a file), as
# "<line>: <text>" pairs, using blame against $CLEANUP_TIP filtered to commits
# that are on the branch but not on $MAIN_REF.
added_returns_for_file() {
    local file="$1"
    git blame --line-porcelain "$CLEANUP_TIP" -- "$file" 2>/dev/null | awk -v shafile="$BRANCH_SHAS" '
        BEGIN { while ((getline s < shafile) > 0) branch[s]=1 }
        /^[0-9a-f]+ [0-9]+ [0-9]+/ { cur=$1; ln=$3; next }
        /^\t/ { if ((cur in branch) && index($0,"@return")>0) { c=$0; sub(/^\t/,"",c); print ln": "c } }
    '
}

# Verify a single commit is a pure @return removal touching only $2 (the file).
# Returns 0 if OK, non-zero otherwise.
verify_commit() {
    local sha="$1" file="$2"
    local changed patch adds dels nret line
    changed="$(git diff-tree --no-commit-id --name-only -r "$sha")"
    [ "$changed" = "$file" ] || return 1
    patch="$(git show --format= --unified=0 "$sha" -- "$file")"
    adds="$(printf '%s\n' "$patch" | grep -E '^\+' | grep -vE '^\+\+\+')"
    [ -z "$adds" ] || return 1                       # no additions allowed
    dels="$(printf '%s\n' "$patch" | grep -E '^-' | grep -vE '^---' | sed 's/^-//')"
    [ -n "$dels" ] || return 1                       # must delete something
    nret="$(printf '%s\n' "$dels" | grep -c '@return')"
    [ "$nret" -eq 1 ] || return 1                    # exactly one @return per commit
    while IFS= read -r line; do
        [ -n "$line" ] || continue
        printf '%s' "$line" | grep -qE '^[[:space:]]*\*' || return 1   # must be a comment line
        printf '%s' "$line" | grep -qE '/\*\*|\*/' && return 1         # never delete comment delimiters
        # a deleted line that begins a tag must be the @return itself, never @param/@tparam/@throws/...
        if printf '%s' "$line" | grep -qE '^[[:space:]]*\*[[:space:]]*@'; then
            printf '%s' "$line" | grep -q '@return' || return 1
        fi
    done <<< "$dels"
    return 0
}

process_file() {
    local file="$1"
    [ -f "$file" ] || return 0
    local added count file_start new c kept
    added="$(added_returns_for_file "$file")"
    [ -n "$added" ] || { log "    no added @return; skip"; return 0; }
    count="$(printf '%s\n' "$added" | grep -c ':')"
    log "    $count added @return tag(s)"

    if [ "$DRY_RUN" = "true" ]; then
        printf '%s\n' "$added" | sed 's/^/      /' | tee -a "$LOG_FILE" >/dev/null
        return 0
    fi

    file_start="$(git rev-parse HEAD)"

    local abs="$REPO_ROOT/$file"
    local prompt="${TEMPLATE_CONTENT//\{FILE_PATH\}/$abs}"
    prompt="${prompt//\{ADDED_RETURNS\}/$added}"

    local safe; safe="$(printf '%s' "$file" | tr '/' '_')"
    local claude_log="$LOG_DIR/${safe}.claude.log"

    local -a claude_args=(--dangerously-skip-permissions -p "$prompt" --allowedTools Edit,Read,Bash)
    [ -n "$MODEL" ] && claude_args+=(--model "$MODEL")

    # Throttle: pause before each real Claude call (skipped for files with no
    # work and for dry runs, which both return before reaching this point).
    if [ "$PAUSE_SECONDS" -gt 0 ]; then
        log "    pausing ${PAUSE_SECONDS}s before Claude call..."
        sleep "$PAUSE_SECONDS"
    fi

    "$CLAUDE_BIN" "${claude_args[@]}" > "$claude_log" 2>&1 \
        || log_error "    claude exited non-zero on $file (see $claude_log)"

    # Verify every new commit; reset from the first violation onward.
    new="$(git rev-list --reverse "$file_start"..HEAD 2>/dev/null)"
    for c in $new; do
        if ! verify_commit "$c" "$file"; then
            log_error "    bad commit $c on $file -- resetting from here, flagging for manual review"
            git reset --quiet --hard "${c}~1"
            echo "$file  (bad commit reverted: $c)" >> "$MANUAL_REVIEW"
            break
        fi
    done

    # Discard any uncommitted leftovers Claude may have left behind.
    if [ -n "$(git status --porcelain)" ]; then
        git checkout --quiet -- . 2>/dev/null
        git reset --quiet --hard HEAD
        log "    discarded uncommitted leftovers on $file"
        echo "$file  (uncommitted leftovers discarded)" >> "$MANUAL_REVIEW"
    fi

    kept="$(git rev-list --count "$file_start"..HEAD)"
    log "    kept $kept removal commit(s)"
    BRANCH_REMOVALS=$((BRANCH_REMOVALS + kept))
}

process_branch() {
    local branch="$1"
    local work="${branch}${WORK_SUFFIX}"
    log "=============================================================="
    log "Branch: $branch  ->  work-branch: $work"

    if ! git rev-parse --verify --quiet "origin/$branch" >/dev/null; then
        log_error "origin/$branch does not exist; skipping"
        return 0
    fi

    if [ "$DRY_RUN" = "true" ]; then
        # Dry run: do not create or check out the work-branch; just report.
        CLEANUP_TIP="$(git rev-parse "origin/$branch")"
    else
        if git rev-parse --verify --quiet "$work" >/dev/null; then
            if [ "$SKIP_EXISTING" = "true" ]; then
                log "work-branch $work already exists; skipping (SKIP_EXISTING=true)"
                return 0
            fi
            git branch -D "$work" >/dev/null
        fi
        git checkout --quiet -B "$work" "origin/$branch" || { log_error "checkout -B $work failed"; return 0; }
        CLEANUP_TIP="$(git rev-parse HEAD)"
    fi

    BRANCH_SHAS="$(mktemp)"
    git rev-list "$MAIN_REF..origin/$branch" > "$BRANCH_SHAS"

    # Files changed by the cleanup commit(s), restricted to source files.
    local files file n=0
    files="$(git diff --name-only "$MAIN_REF...origin/$branch" -- '*.scala' '*.java')"
    BRANCH_REMOVALS=0
    for file in $files; do
        if [ "$MAX_FILES_PER_BRANCH" -gt 0 ] && [ "$n" -ge "$MAX_FILES_PER_BRANCH" ]; then
            log "  reached MAX_FILES_PER_BRANCH=$MAX_FILES_PER_BRANCH; stopping this branch"
            break
        fi
        log "  file: $file"
        process_file "$file"
        n=$((n + 1))
    done

    rm -f "$BRANCH_SHAS"
    log "Branch $work: $BRANCH_REMOVALS total removal commit(s) across $n file(s)"
    printf '%-55s %s removals\n' "$work" "$BRANCH_REMOVALS" >> "$SUMMARY"
    [ "$DRY_RUN" = "true" ] || CREATED_BRANCHES+=("$work")
}

# ---- main -------------------------------------------------------------------
log "=============================================================="
log "remove-redundant-returns.sh starting (DRY_RUN=$DRY_RUN)"
log "=============================================================="

CREATED_BRANCHES=()
bn=0
for branch in $BRANCHES; do
    if [ "$MAX_BRANCHES" -gt 0 ] && [ "$bn" -ge "$MAX_BRANCHES" ]; then
        log "Reached MAX_BRANCHES=$MAX_BRANCHES; stopping."
        break
    fi
    # Pause between branches (not before the first, not during dry runs).
    if [ "$bn" -gt 0 ] && [ "$DRY_RUN" != "true" ] && [ "$BRANCH_PAUSE_SECONDS" -gt 0 ]; then
        log "Pausing ${BRANCH_PAUSE_SECONDS}s before next branch..."
        sleep "$BRANCH_PAUSE_SECONDS"
    fi
    process_branch "$branch"
    bn=$((bn + 1))
done

# Return to where we started.
git checkout --quiet "$START_REF" 2>/dev/null || true

log ""
log "=============================================================="
log "DONE. Per-branch removal counts:"
sed 's/^/  /' "$SUMMARY" | tee -a "$LOG_FILE"
if [ -s "$MANUAL_REVIEW" ]; then
    log ""
    log "Files flagged for manual review (in $MANUAL_REVIEW):"
    sed 's/^/  /' "$MANUAL_REVIEW" | tee -a "$LOG_FILE"
fi
log ""
log "Nothing was pushed. To publish the work-branches, run:"
if [ "${#CREATED_BRANCHES[@]}" -gt 0 ]; then
    for b in "${CREATED_BRANCHES[@]}"; do
        log "  git push -u origin $b"
    done
else
    log "  (no work-branches were created)"
fi
log "=============================================================="
