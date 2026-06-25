#!/bin/bash

# =============================================================================
# fill-doc-todos.sh
#
# Fill in `TODO FILL IN` Scaladoc placeholders (including the hard whole-comment
# descriptions) using a diverse, multi-round writer/reviewer loop:
#
#   Writer (Opus) drafts
#   repeat up to MAX_ROUNDS:
#       Accuracy review (Codex, different model family)  ┐ run in parallel
#       Style review    (Claude Sonnet)                  ┘
#       if BOTH approve -> done
#       else Writer refines, addressing both reviews (with right-of-reply)
#   on non-convergence -> keep best draft, flag NEEDS-HUMAN in the digest
#
# This script does NOT commit. It edits the working tree and writes a per-file
# review JSON + a human digest sorted most-needs-review-first. Intended for
# iterating on the process; throw away changes with `git checkout -- <files>`.
#
# Usage:
#   ./fill-doc-todos.sh [file ...]          # specific files
#   ./fill-doc-todos.sh                     # default: io files with markers
#
# Env overrides:
#   MAX_ROUNDS=2  WRITER_MODEL=opus  STYLE_MODEL=sonnet  DRY_RUN=false
# =============================================================================

set -uo pipefail

MAX_ROUNDS=${MAX_ROUNDS:-2}
WRITER_MODEL=${WRITER_MODEL:-opus}
STYLE_MODEL=${STYLE_MODEL:-sonnet}
DRY_RUN=${DRY_RUN:-false}
MARKER="TODO FILL IN"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TODO_WRITER_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
REPO_ROOT="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
PROMPTS_DIR="$SCRIPT_DIR/prompts"
SCHEMA="$SCRIPT_DIR/schemas/doc-review.schema.json"
REVIEWS_DIR="$TODO_WRITER_DIR/reviews"
LOG_FILE="$TODO_WRITER_DIR/fill-doc-todos.log"
WORK_DIR="$(mktemp -d)"
trap 'rm -rf "$WORK_DIR"' EXIT

mkdir -p "$REVIEWS_DIR"

log() { local m="[$(date '+%H:%M:%S')] $1"; echo "$m"; echo "$m" >> "$LOG_FILE"; }

# Render a prompt file, substituting {FILE_PATH}.
render() { sed "s|{FILE_PATH}|$1|g" "$2"; }

# Extract the review JSON text from a raw model emission (strip ``` fences).
clean_json() { sed -e 's/^```json//' -e 's/^```//' | sed '/^[[:space:]]*$/d'; }

# Default target list: io files that still contain the marker.
if [ "$#" -gt 0 ]; then
    TARGETS=("$@")
else
    mapfile -t TARGETS < <(grep -rl "$MARKER" "$REPO_ROOT/library/src/scala/io/" 2>/dev/null)
fi

if [ "${#TARGETS[@]}" -eq 0 ]; then
    log "No files with '$MARKER' found. Nothing to do."
    exit 0
fi

log "=============================================="
log "fill-doc-todos.sh starting"
log "Files: ${#TARGETS[@]} | rounds: $MAX_ROUNDS | writer: $WRITER_MODEL | style: $STYLE_MODEL"
log "=============================================="

for FILE in "${TARGETS[@]}"; do
    # Normalize to absolute path.
    case "$FILE" in /*) ABS="$FILE" ;; *) ABS="$REPO_ROOT/$FILE" ;; esac
    REL="${ABS#"$REPO_ROOT"/}"
    SAFE="$(echo "$REL" | tr '/' '_')"

    if ! grep -q "$MARKER" "$ABS" 2>/dev/null; then
        log "SKIP $REL (no '$MARKER')"
        continue
    fi

    n_main=$(grep -cE '/\*\* *'"$MARKER" "$ABS")
    n_tags=$(grep -cE '@(param|tparam|return).*'"$MARKER" "$ABS")
    log ""
    log "### $REL  ($n_main description, $n_tags tag placeholders)"

    if [ "$DRY_RUN" = "true" ]; then
        log "DRY RUN: would fill+review $REL"
        continue
    fi

    # Snapshot the file BEFORE the writer runs so reviewers can be scoped to
    # exactly the writer's changes (not pre-existing docs or todo-writer markers).
    ORIG="$WORK_DIR/${SAFE}.orig"
    cp -f "$ABS" "$ORIG"

    # ---- Writer: initial draft ---------------------------------------------
    log "  writer: drafting ($WRITER_MODEL)..."
    render "$ABS" "$PROMPTS_DIR/doc-writer-prompt.txt" \
        | claude --dangerously-skip-permissions -p --model "$WRITER_MODEL" \
            --allowedTools Edit,Read,Grep,Glob \
            > "$REVIEWS_DIR/${SAFE}.writer.log" 2>&1
    if [ $? -ne 0 ]; then
        log "  ERROR writer failed; see ${SAFE}.writer.log — skipping file"
        continue
    fi

    ACC_JSON="$WORK_DIR/${SAFE}.acc.json"
    STY_JSON="$WORK_DIR/${SAFE}.sty.json"
    final_acc="$REVIEWS_DIR/${SAFE}.accuracy.json"
    final_sty="$REVIEWS_DIR/${SAFE}.style.json"

    round=1
    converged=false
    while [ "$round" -le "$MAX_ROUNDS" ]; do
        log "  round $round/$MAX_ROUNDS: accuracy (Codex) ‖ style ($STYLE_MODEL)..."

        # Unified diff of ONLY the writer's changes, to scope both reviewers.
        DIFF_BLOCK="$WORK_DIR/${SAFE}.diff"
        { echo; echo "=== DIFF OF DOCS TO REVIEW (judge only these additions) ==="
          diff -u "$ORIG" "$ABS" || true
        } > "$DIFF_BLOCK"

        # Accuracy review (Codex) — background.
        ( cat <(render "$ABS" "$PROMPTS_DIR/doc-accuracy-review-prompt.txt") "$DIFF_BLOCK" \
            | codex exec -s read-only --skip-git-repo-check -C "$REPO_ROOT" \
                --output-schema "$SCHEMA" --output-last-message "$ACC_JSON" - \
                > "$REVIEWS_DIR/${SAFE}.accuracy.log" 2>&1 ) &
        acc_pid=$!

        # Style review (Claude Sonnet) — background.
        ( cat <(render "$ABS" "$PROMPTS_DIR/doc-style-review-prompt.txt") "$DIFF_BLOCK" \
            | claude --dangerously-skip-permissions -p --model "$STYLE_MODEL" \
                --allowedTools Read,Grep,Glob --output-format json \
                > "$WORK_DIR/${SAFE}.sty.raw" 2>"$REVIEWS_DIR/${SAFE}.style.log"
          jq -r '.result' "$WORK_DIR/${SAFE}.sty.raw" 2>/dev/null | clean_json > "$STY_JSON" ) &
        sty_pid=$!

        wait "$acc_pid"; wait "$sty_pid"

        cp -f "$ACC_JSON" "$final_acc" 2>/dev/null || echo '{}' > "$final_acc"
        cp -f "$STY_JSON" "$final_sty" 2>/dev/null || echo '{}' > "$final_sty"

        acc_verdict=$(jq -r '.verdict // "revise"' "$final_acc" 2>/dev/null || echo revise)
        sty_verdict=$(jq -r '.verdict // "revise"' "$final_sty" 2>/dev/null || echo revise)
        log "    accuracy=$acc_verdict  style=$sty_verdict"

        if [ "$acc_verdict" = "approve" ] && [ "$sty_verdict" = "approve" ]; then
            converged=true
            break
        fi

        if [ "$round" -eq "$MAX_ROUNDS" ]; then
            break   # no refine after the last review; fall through to flagging
        fi

        # ---- Writer: refine using both reviews -----------------------------
        log "    refine: incorporating both reviews..."
        { render "$ABS" "$PROMPTS_DIR/doc-refine-prompt.txt"
          echo "--- ACCURACY (Codex) ---"; cat "$final_acc"
          echo "--- STYLE ($STYLE_MODEL) ---"; cat "$final_sty"
        } | claude --dangerously-skip-permissions -p --model "$WRITER_MODEL" \
                --allowedTools Edit,Read,Grep,Glob \
                > "$REVIEWS_DIR/${SAFE}.refine${round}.log" 2>&1

        round=$((round + 1))
    done

    # ---- Aggregate digest (most-needs-review first) ------------------------
    DIGEST="$REVIEWS_DIR/${SAFE}.digest.md"
    {
        echo "# Doc review digest: $REL"
        echo
        echo "- converged: $converged (after up to $MAX_ROUNDS rounds)"
        echo "- accuracy verdict: $(jq -r '.verdict // "?"' "$final_acc" 2>/dev/null)"
        echo "- style verdict: $(jq -r '.verdict // "?"' "$final_sty" 2>/dev/null)"
        echo
        echo "## Needs human / low confidence (check these first)"
        echo
        jq -rs '
          (.[0].items // []) + (.[1].items // [])
          | map(select(.needs_human == true or .confidence == "low" or .severity == "blocker"))
          | sort_by(.needs_human != true, .confidence != "low", .severity != "blocker")
          | .[] | "- L\(.line) `\(.symbol)` [\(.severity)/\(.confidence)\(if .needs_human then "/NEEDS-HUMAN" else "" end)]: \(.issue) \(if .suggestion != "" then "→ " + .suggestion else "" end)"
        ' "$final_acc" "$final_sty" 2>/dev/null || echo "(could not parse reviews)"
        echo
        echo "## Inline NEEDS-HUMAN markers left in source"
        grep -nE "NEEDS-HUMAN" "$ABS" 2>/dev/null | sed 's/^/- L/' || echo "(none)"
    } > "$DIGEST"

    # ---- Bonus findings: real bugs noticed in PRE-EXISTING docs ------------
    BONUS="$REVIEWS_DIR/bonus-findings.md"
    n_bonus=$(jq -r '(.bonus_findings // []) | length' "$final_acc" 2>/dev/null || echo 0)
    if [ "${n_bonus:-0}" -gt 0 ]; then
        {
            echo "## $REL"
            jq -r '(.bonus_findings // [])[]
                | "- L\(.line) `\(.symbol)`: \(.issue) → \(.suggestion)"' "$final_acc" 2>/dev/null
            echo
        } >> "$BONUS"
        log "  bonus: $n_bonus pre-existing doc issue(s) noted in $BONUS"
    fi

    flagged=$(grep -cE "NEEDS-HUMAN" "$ABS" 2>/dev/null); flagged=${flagged:-0}
    remaining=$(grep -c "$MARKER" "$ABS" 2>/dev/null); remaining=${remaining:-0}
    log "  done: converged=$converged | NEEDS-HUMAN=$flagged | unfilled markers left=$remaining"
    log "  digest: $DIGEST"
done

log ""
log "=============================================="
log "COMPLETE. Reviews + digests in: $REVIEWS_DIR"
log "Changes are in the working tree, UNCOMMITTED. Throw away with:"
log "  git checkout -- <files>"
log "=============================================="
