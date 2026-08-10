#!/bin/bash

# =============================================================================
# fill-doc-todos-poolside.sh
#
# Variant of fill-doc-todos.sh that moves the WRITER onto a free-tier model and
# adds an ADJUDICATOR. The original script is untouched: it produced PRs #26429,
# #26657 and #26669 and remains the known-good path.
#
#   Writer (pool / poolside laguna) drafts
#   repeat up to MAX_ROUNDS:
#       Codex review (accuracy emphasis)  ┐ run in parallel
#       Claude review (style emphasis)     ┘
#       Adjudicator (poolside) merges the two into ONE verdict + ONE worklist
#       if adjudicator approves -> done
#       else Writer refines against the adjudicated worklist
#   after a non-converged final review -> one final Writer refinement, then move on
#
# Differences from fill-doc-todos.sh, and why:
#
#   1. WRITER is `pool exec` (poolside's own agent) rather than `claude -p`.
#      Five earlier client trials are recorded in docs/aider-notes.md section 9;
#      pool was the only one that filled a file without corrupting it, because it
#      edits incrementally instead of regenerating the file.
#
#   2. STYLE reviewer is Opus rather than Sonnet.
#
#   3. NEW adjudicator step. Previously convergence was a shell AND over the two
#      verdicts, and the writer had to reconcile two possibly-contradictory
#      reviews by itself. The adjudicator merges duplicates, settles conflicts,
#      drops reviewer points that the code refutes, and issues the final verdict.
#      It runs as a plain JSON-mode API call, not an agent: it edits nothing.
#
#   4. CODE-INTEGRITY GATE after every writer and refine step. Run 4 of the client
#      trials silently deleted an entire public class while reporting success on
#      the marker count, the edit count and the diffstat. Only a strip-comments
#      diff caught it. Any step that changes a non-comment line has its file
#      reverted and the file is abandoned with a loud log line.
#
# This script does NOT commit. It edits the working tree.
#
# Usage:
#   ./fill-doc-todos-poolside.sh [file ...]
#
# Env overrides:
#   MAX_ROUNDS=2
#   WRITER_MODEL=poolside/laguna-s-2.1     (pool, standalone mode)
#   STYLE_MODEL=opus                       (claude -p)
#   ACCURACY_MODEL=gpt-5.6-terra           (codex exec)
#   ADJUDICATOR_MODEL=poolside/laguna-s-2.1
#   POOLSIDE_ENV_FILE=/home/node/.aider/.env
#   WRITER_TIMEOUT=3600  INTER_FILE_PAUSE_SECONDS=60
#   DRY_RUN=false  STOP_FILE=todo-writer/stop-fill-doc-todos
# =============================================================================

set -uo pipefail

MAX_ROUNDS=${MAX_ROUNDS:-2}
WRITER_MODEL=${WRITER_MODEL:-poolside/laguna-s-2.1}
STYLE_MODEL=${STYLE_MODEL:-opus}
ACCURACY_MODEL=${ACCURACY_MODEL:-gpt-5.6-terra}
ADJUDICATOR_MODEL=${ADJUDICATOR_MODEL:-poolside/laguna-s-2.1}
WRITER_TIMEOUT=${WRITER_TIMEOUT:-3600}
DRY_RUN=${DRY_RUN:-false}
MARKER="TODO FILL IN"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TODO_WRITER_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
REPO_ROOT="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
PROMPTS_DIR="$SCRIPT_DIR/prompts"
SCHEMA="$SCRIPT_DIR/schemas/doc-review.schema.json"
ADJ_SCHEMA="$SCRIPT_DIR/schemas/doc-adjudication.schema.json"
REVIEWS_DIR="$TODO_WRITER_DIR/reviews"
LOG_FILE="$TODO_WRITER_DIR/fill-doc-todos-poolside.log"
INTER_FILE_PAUSE_SECONDS=${INTER_FILE_PAUSE_SECONDS:-60}
STOP_FILE=${STOP_FILE:-"$TODO_WRITER_DIR/stop-fill-doc-todos"}
WORK_DIR="$(mktemp -d)"
trap 'rm -rf "$WORK_DIR"' EXIT

mkdir -p "$REVIEWS_DIR"

# ---- poolside credentials --------------------------------------------------
# The key lives on the persisted volume; see docs/aider-notes.md section 3 for
# why it is there and not in the repo or in .zshrc.
POOLSIDE_ENV_FILE=${POOLSIDE_ENV_FILE:-/home/node/.aider/.env}
if [ -r "$POOLSIDE_ENV_FILE" ]; then
    set -a; . "$POOLSIDE_ENV_FILE"; set +a
fi
: "${POOLSIDE_API_KEY:=${OPENAI_API_KEY:-}}"
: "${POOLSIDE_BASE_URL:=${OPENAI_API_BASE:-https://inference.poolside.ai/v1}}"
export POOLSIDE_API_KEY
export POOLSIDE_STANDALONE_BASE_URL="${POOLSIDE_STANDALONE_BASE_URL:-https://inference.poolside.ai}"
export POOLSIDE_STANDALONE_MODEL="$WRITER_MODEL"

if [ -z "$POOLSIDE_API_KEY" ]; then
    echo "No POOLSIDE_API_KEY (looked in $POOLSIDE_ENV_FILE). Aborting." >&2
    exit 2
fi
for tool in pool codex claude jq curl; do
    command -v "$tool" >/dev/null || { echo "Missing required tool: $tool" >&2; exit 2; }
done

log() { local m="[$(date '+%H:%M:%S')] $1"; echo "$m"; echo "$m" >> "$LOG_FILE"; }

between_files() {
    local index=$1
    if [ -e "$STOP_FILE" ]; then
        log "STOP: found $STOP_FILE after completing a file; exiting cleanly."
        exit 0
    fi
    if [ "$index" -lt $(( ${#TARGETS[@]} - 1 )) ] && [ "$INTER_FILE_PAUSE_SECONDS" -gt 0 ]; then
        log "Pausing ${INTER_FILE_PAUSE_SECONDS}s before the next file."
        sleep "$INTER_FILE_PAUSE_SECONDS"
        if [ -e "$STOP_FILE" ]; then
            log "STOP: found $STOP_FILE after the pause; exiting cleanly."
            exit 0
        fi
    fi
}

render() { sed "s|{FILE_PATH}|$1|g" "$2"; }

clean_json() { sed -e 's/^```json//' -e 's/^```//' | awk '/^[[:space:]]*\{/{f=1} f'; }

HOUSE_RULES_FILE="${HOUSE_RULES_FILE:-$TODO_WRITER_DIR/docs/house-rules.md}"
house_rules() {
  if [ -s "$HOUSE_RULES_FILE" ]; then
    echo
    echo "=== LEARNED HOUSE RULES (from reviewer feedback on earlier PRs; apply these) ==="
    cat "$HOUSE_RULES_FILE"
  fi
}

# Non-comment, non-blank lines. Two files with identical output here differ only
# in Scaladoc. This is the gate that catches a writer silently dropping code.
code_lines() { grep -vE '^\s*(\*|/\*\*|\*/)' "$1" | grep -v '^\s*$'; }

# Run the poolside agent over one file. $1 = file, $2 = prompt on stdin path,
# $3 = log destination.
run_pool() {
    local abs=$1 prompt=$2 dest=$3
    timeout --signal=TERM "$WRITER_TIMEOUT" \
        pool exec -f "$prompt" -d "$REPO_ROOT" --unsafe-auto-allow \
        > "$dest" 2>&1
}

# Adjudicate two reviews into one verdict. Plain JSON-mode chat call: no tools,
# no file access, nothing to corrupt.
adjudicate() {
    local abs=$1 acc=$2 sty=$3 diff_block=$4 out=$5
    local sys_prompt user_payload req
    sys_prompt="$( { render "$abs" "$PROMPTS_DIR/doc-adjudicator-prompt.txt"; house_rules
                     echo; echo "=== ADJUDICATION SCHEMA (conform exactly) ==="; cat "$ADJ_SCHEMA"; } )"
    user_payload="$( { cat "$diff_block"
                       echo; echo "=== ACCURACY REVIEW (JSON) ==="; cat "$acc"
                       echo; echo "=== STYLE REVIEW (JSON) ==="; cat "$sty"; } )"
    req="$WORK_DIR/adj-req.json"
    jq -n --arg m "$ADJUDICATOR_MODEL" --arg s "$sys_prompt" --arg u "$user_payload" \
      '{model:$m, max_tokens:8000, response_format:{type:"json_object"},
        messages:[{role:"system",content:$s},{role:"user",content:$u}]}' > "$req"
    curl -s --max-time 900 "$POOLSIDE_BASE_URL/chat/completions" \
        -H "Authorization: Bearer $POOLSIDE_API_KEY" -H "Content-Type: application/json" \
        --data @"$req" \
      | jq -r '.choices[0].message.content // empty' | clean_json > "$out"
    [ -s "$out" ] || echo '{}' > "$out"
}

if [ "$#" -gt 0 ]; then
    TARGETS=("$@")
else
    log "No files given. Pass the target files explicitly."
    exit 2
fi

log "=============================================="
log "fill-doc-todos-poolside.sh starting"
log "Files: ${#TARGETS[@]} | rounds: $MAX_ROUNDS"
log "writer: pool/$WRITER_MODEL | accuracy: codex/$ACCURACY_MODEL | style: claude/$STYLE_MODEL"
log "adjudicator: $ADJUDICATOR_MODEL (JSON-mode API, no tools)"
log "=============================================="

for index in "${!TARGETS[@]}"; do
    FILE="${TARGETS[$index]}"
    case "$FILE" in /*) ABS="$FILE" ;; *) ABS="$REPO_ROOT/$FILE" ;; esac
    REL="${ABS#"$REPO_ROOT"/}"
    SAFE="$(echo "$REL" | tr '/' '_')"

    if ! grep -q "$MARKER" "$ABS" 2>/dev/null; then
        log "SKIP $REL (no '$MARKER')"; between_files "$index"; continue
    fi

    n_main=$(grep -cE '/\*\* *'"$MARKER" "$ABS")
    n_tags=$(grep -cE '@(param|tparam|return).*'"$MARKER" "$ABS")
    log ""
    log "### $REL  ($n_main description, $n_tags tag placeholders)"

    if [ "$DRY_RUN" = "true" ]; then
        log "DRY RUN: would fill+review $REL"; between_files "$index"; continue
    fi

    ORIG="$WORK_DIR/${SAFE}.orig"
    CODE_BEFORE="$WORK_DIR/${SAFE}.code"
    cp -f "$ABS" "$ORIG"
    code_lines "$ABS" > "$CODE_BEFORE"

    # Revert and abandon the file if a step touched anything but comments.
    integrity_ok() {
        local stage=$1
        if code_lines "$ABS" | diff -q "$CODE_BEFORE" - >/dev/null; then
            return 0
        fi
        log "  !! CODE-INTEGRITY FAILURE after $stage on $REL"
        log "  !! a non-comment line changed; reverting the file and abandoning it"
        code_lines "$ABS" | diff "$CODE_BEFORE" - | head -20 | while read -r l; do log "  !!   $l"; done
        cp -f "$ORIG" "$ABS"
        return 1
    }

    # ---- Writer -------------------------------------------------------------
    log "  writer: drafting (pool / $WRITER_MODEL)..."
    render "$ABS" "$PROMPTS_DIR/doc-writer-prompt-agent.txt" > "$WORK_DIR/${SAFE}.wprompt"
    house_rules >> "$WORK_DIR/${SAFE}.wprompt"
    run_pool "$ABS" "$WORK_DIR/${SAFE}.wprompt" "$REVIEWS_DIR/${SAFE}.writer.log"
    integrity_ok "writer" || { between_files "$index"; continue; }

    ACC_JSON="$WORK_DIR/${SAFE}.acc.json"
    STY_JSON="$WORK_DIR/${SAFE}.sty.json"
    final_acc="$REVIEWS_DIR/${SAFE}.accuracy.json"
    final_sty="$REVIEWS_DIR/${SAFE}.style.json"
    final_adj="$REVIEWS_DIR/${SAFE}.adjudication.json"

    round=1
    converged=false
    final_refinement=false
    while [ "$round" -le "$MAX_ROUNDS" ]; do
        log "  round $round/$MAX_ROUNDS: Codex ($ACCURACY_MODEL, accuracy) ‖ Claude ($STYLE_MODEL, style)..."

        DIFF_BLOCK="$WORK_DIR/${SAFE}.diff"
        { echo; echo "=== DIFF OF DOCS TO REVIEW (judge only these additions) ==="
          diff -u "$ORIG" "$ABS" || true
        } > "$DIFF_BLOCK"

        ( cat <(render "$ABS" "$PROMPTS_DIR/doc-accuracy-review-prompt.txt") <(house_rules) "$DIFF_BLOCK" \
            | codex exec --model "$ACCURACY_MODEL" -s read-only --skip-git-repo-check -C "$REPO_ROOT" \
                --output-schema "$SCHEMA" --output-last-message "$ACC_JSON" - \
                > "$REVIEWS_DIR/${SAFE}.accuracy.log" 2>&1 ) &
        acc_pid=$!

        # The schema is piped in explicitly: `claude -p` has no --output-schema,
        # so otherwise "conforming to the provided schema" refers to a schema the
        # model never sees, and it invents its own item shape. See the same fix
        # in fill-doc-todos.sh.
        ( cat <(render "$ABS" "$PROMPTS_DIR/doc-style-review-prompt.txt") <(house_rules) \
              <(echo; echo "=== SCHEMA (conform exactly) ==="; cat "$SCHEMA") "$DIFF_BLOCK" \
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
        log "    reviewers: codex=$acc_verdict  claude=$sty_verdict"

        # ---- Adjudicator ---------------------------------------------------
        log "    adjudicating (poolside / $ADJUDICATOR_MODEL)..."
        adjudicate "$ABS" "$final_acc" "$final_sty" "$DIFF_BLOCK" "$final_adj"
        adj_verdict=$(jq -r '.verdict // "revise"' "$final_adj" 2>/dev/null || echo revise)
        n_items=$(jq -r '(.resolved_items // []) | length' "$final_adj" 2>/dev/null || echo 0)
        n_disagree=$(jq -r '(.disagreements // []) | length' "$final_adj" 2>/dev/null || echo 0)
        log "    adjudicator: $adj_verdict  (${n_items} item(s), ${n_disagree} disagreement(s) settled)"

        if [ "$adj_verdict" = "approve" ]; then
            converged=true
            break
        fi

        if [ "$round" -eq "$MAX_ROUNDS" ]; then
            final_refinement=true
            log "    final refine: working the adjudicated list (not re-reviewed)..."
        else
            log "    refine: working the adjudicated list..."
        fi

        { render "$ABS" "$PROMPTS_DIR/doc-refine-prompt-agent.txt"; house_rules
          echo; cat "$final_adj"
        } > "$WORK_DIR/${SAFE}.rprompt"
        run_pool "$ABS" "$WORK_DIR/${SAFE}.rprompt" "$REVIEWS_DIR/${SAFE}.refine${round}.log"
        integrity_ok "refine round $round" || break

        [ "$final_refinement" = "true" ] && break
        round=$((round + 1))
    done

    # ---- Digest -------------------------------------------------------------
    DIGEST="$REVIEWS_DIR/${SAFE}.digest.md"
    {
        echo "# Doc review digest: $REL"
        echo
        echo "- converged: $converged (after up to $MAX_ROUNDS rounds)"
        echo "- final refinement after review limit: $final_refinement (not re-reviewed)"
        echo "- Codex verdict (accuracy): $(jq -r '.verdict // "?"' "$final_acc" 2>/dev/null)"
        echo "- Claude verdict (style): $(jq -r '.verdict // "?"' "$final_sty" 2>/dev/null)"
        echo "- ADJUDICATOR verdict (final): $(jq -r '.verdict // "?"' "$final_adj" 2>/dev/null)"
        echo
        echo "## Reviewer disagreements the adjudicator settled"
        echo
        jq -r '(.disagreements // [])[]
          | "- L\(.line) `\(.symbol)` -> ruled for **\(.ruling)**\n  - accuracy: \(.accuracy_position)\n  - style: \(.style_position)\n  - why: \(.rationale)"' \
          "$final_adj" 2>/dev/null || echo "(none / unparseable)"
        echo
        echo "## Outstanding worklist at the end"
        echo
        jq -r '(.resolved_items // [])
          | sort_by(.needs_human != true, .severity != "blocker")
          | .[] | "- L\(.line) `\(.symbol)` [\(.severity)/\(.raised_by)\(if .needs_human then "/NEEDS-HUMAN" else "" end)]: \(.instruction)"' \
          "$final_adj" 2>/dev/null || echo "(none / unparseable)"
        echo
        echo "## Inline NEEDS-HUMAN markers left in source"
        grep -nE "NEEDS-HUMAN" "$ABS" 2>/dev/null | sed 's/^/- L/' || echo "(none)"
    } > "$DIGEST"

    BONUS="$REVIEWS_DIR/bonus-findings.md"
    n_bonus=$(jq -rs 'map(.bonus_findings // []) | add | length' "$final_acc" "$final_sty" 2>/dev/null || echo 0)
    if [ "${n_bonus:-0}" -gt 0 ]; then
        {
            echo "## $REL"
            jq -rs 'map(.bonus_findings // []) | add | .[]
                | "- L\(.line) `\(.symbol)`: \(.issue) → \(.suggestion)"' "$final_acc" "$final_sty" 2>/dev/null
            echo
        } >> "$BONUS"
        log "  bonus: $n_bonus pre-existing doc issue(s) noted in $BONUS"
    fi

    flagged=$(grep -cE "NEEDS-HUMAN" "$ABS" 2>/dev/null); flagged=${flagged:-0}
    remaining=$(grep -c "$MARKER" "$ABS" 2>/dev/null); remaining=${remaining:-0}
    log "  done: converged=$converged | NEEDS-HUMAN=$flagged | unfilled markers left=$remaining"
    log "  digest: $DIGEST"
    between_files "$index"
done

log ""
log "=============================================="
log "COMPLETE. Reviews + digests in: $REVIEWS_DIR"
log "Changes are in the working tree, UNCOMMITTED."
log "=============================================="
