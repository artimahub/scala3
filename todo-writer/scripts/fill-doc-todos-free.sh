#!/bin/bash

# =============================================================================
# fill-doc-todos-free.sh
#
# Every role on a FREE model. No Claude Code, no Codex, no subscription spend.
#
#   Writer      (Mistral  zai-glm-5-2)          drafts
#   repeat up to MAX_ROUNDS:
#       Accuracy review (Cerebras gemma-4-31b)  ┐ run in parallel
#       Style review    (Mistral  mistral-medium)┘
#       Adjudicator     (Mistral  zai-glm-5-2)  merges both into ONE verdict
#       if adjudicator approves -> done
#       else Writer refines against the adjudicated worklist
#
# Model choices come from 26 writer trials on the same input; see
# docs/../experiments/RESULTS.md. Briefly:
#
#   zai-glm-5-2    1 redundant @return and both accuracy probes right -- the
#                  best score seen anywhere, free, 23s for a 42-declaration file
#   gemma-4-31b    passes both accuracy probes; a DIFFERENT provider and family
#                  from the writer, which is what makes its dissent meaningful
#   mistral-medium 2 redundant @return, the best convention-follower after the
#                  writer itself
#
# Three families across two providers. Reviewer independence matters more than
# raw reviewer strength: two reviewers that share weights agree for the wrong
# reasons.
#
# NOTE on writer == adjudicator: same weights means same blind spots, so the
# adjudicator is least likely to overrule the writer exactly where the writer is
# wrong. Accepted deliberately -- it arbitrates BETWEEN reviewers rather than
# re-reviewing the diff -- but it is the weakest link in this arrangement.
#
# All four roles are plain JSON-mode HTTP calls except the writer, which uses
# direct-writer.py. No coding-agent client is involved anywhere. Of the four
# clients tried (aider, Codex CLI, pool, direct), only `direct` worked for every
# model; aider alone broke three of four. See RESULTS.md.
#
# This script does NOT commit. It edits the working tree.
#
# Usage:
#   ./fill-doc-todos-free.sh <file> [file ...]
#
# Control files (create/remove while it runs):
#   PAUSE  -> todo-writer/PAUSE   waits at the next phase boundary, rechecking
#             every PAUSE_SLEEP seconds until the file is removed
#   STOP   -> todo-writer/stop-fill-doc-todos   exits cleanly after the current
#             file completes
#
# Env overrides:
#   MAX_ROUNDS=2
#   WRITER_MODEL=zai-glm-5-2          WRITER_PROVIDER=mistral
#   ACCURACY_MODEL=gemma-4-31b        ACCURACY_PROVIDER=cerebras
#   STYLE_MODEL=mistral-medium-latest STYLE_PROVIDER=mistral
#   ADJUDICATOR_MODEL=zai-glm-5-2     ADJUDICATOR_PROVIDER=mistral
#   INTER_FILE_PAUSE_SECONDS=60  PAUSE_SLEEP=1200  MAX_TOKENS=32000
#   DRY_RUN=false
# =============================================================================

set -uo pipefail

MAX_ROUNDS=${MAX_ROUNDS:-2}
WRITER_MODEL=${WRITER_MODEL:-zai-glm-5-2};                WRITER_PROVIDER=${WRITER_PROVIDER:-mistral}
ACCURACY_MODEL=${ACCURACY_MODEL:-gemma-4-31b};            ACCURACY_PROVIDER=${ACCURACY_PROVIDER:-cerebras}
STYLE_MODEL=${STYLE_MODEL:-mistral-medium-latest};        STYLE_PROVIDER=${STYLE_PROVIDER:-mistral}
ADJUDICATOR_MODEL=${ADJUDICATOR_MODEL:-zai-glm-5-2};      ADJUDICATOR_PROVIDER=${ADJUDICATOR_PROVIDER:-mistral}
MAX_TOKENS=${MAX_TOKENS:-32000}
DRY_RUN=${DRY_RUN:-false}
MARKER="TODO FILL IN"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TODO_WRITER_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
REPO_ROOT="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
PROMPTS_DIR="$SCRIPT_DIR/prompts"
SCHEMA="$SCRIPT_DIR/schemas/doc-review.schema.json"
ADJ_SCHEMA="$SCRIPT_DIR/schemas/doc-adjudication.schema.json"
DIRECT_WRITER="$TODO_WRITER_DIR/experiments/direct-writer.py"
REVIEWS_DIR="$TODO_WRITER_DIR/reviews"
LOG_FILE="$TODO_WRITER_DIR/fill-doc-todos-free.log"

INTER_FILE_PAUSE_SECONDS=${INTER_FILE_PAUSE_SECONDS:-60}
PAUSE_FILE=${PAUSE_FILE:-"$TODO_WRITER_DIR/PAUSE"}
PAUSE_SLEEP=${PAUSE_SLEEP:-1200}
STOP_FILE=${STOP_FILE:-"$TODO_WRITER_DIR/stop-fill-doc-todos"}
WORK_DIR="$(mktemp -d)"
trap 'rm -rf "$WORK_DIR"' EXIT

mkdir -p "$REVIEWS_DIR"

# ---- credentials -----------------------------------------------------------
ENV_FILE=${ENV_FILE:-/home/node/.aider/.env}
[ -r "$ENV_FILE" ] && { set -a; . "$ENV_FILE"; set +a; }

# Map a provider name to its base URL and key. Keeping this in one place is what
# lets any role be moved to any provider by changing two env vars.
provider_base() {
    case "$1" in
        mistral)    echo "${MISTRAL_API_BASE:-https://api.mistral.ai/v1}" ;;
        cerebras)   echo "${CEREBRAS_API_BASE:-https://api.cerebras.ai/v1}" ;;
        openrouter) echo "${OPENROUTER_API_BASE:-https://openrouter.ai/api/v1}" ;;
        poolside)   echo "${OPENAI_API_BASE:-https://inference.poolside.ai/v1}" ;;
        *) echo "" ;;
    esac
}
provider_key() {
    case "$1" in
        mistral)    echo "${MISTRAL_API_KEY:-}" ;;
        cerebras)   echo "${CEREBRAS_API_KEY:-}" ;;
        openrouter) echo "${OPENROUTER_API_KEY:-}" ;;
        poolside)   echo "${OPENAI_API_KEY:-}" ;;
        *) echo "" ;;
    esac
}

for role in "$WRITER_PROVIDER:$WRITER_MODEL" "$ACCURACY_PROVIDER:$ACCURACY_MODEL" \
            "$STYLE_PROVIDER:$STYLE_MODEL" "$ADJUDICATOR_PROVIDER:$ADJUDICATOR_MODEL"; do
    prov="${role%%:*}"
    [ -n "$(provider_base "$prov")" ] || { echo "Unknown provider: $prov" >&2; exit 2; }
    [ -n "$(provider_key  "$prov")" ] || { echo "No API key for provider '$prov' (looked in $ENV_FILE)" >&2; exit 2; }
done
for tool in jq curl python3; do
    command -v "$tool" >/dev/null || { echo "Missing required tool: $tool" >&2; exit 2; }
done
[ -x "$DIRECT_WRITER" ] || { echo "Missing or non-executable: $DIRECT_WRITER" >&2; exit 2; }

log() { local m="[$(date '+%H:%M:%S')] $1"; echo "$m"; echo "$m" >> "$LOG_FILE"; }

# ---- PAUSE -----------------------------------------------------------------
# Checked at every phase boundary, not just between files, so a long partition
# can be halted mid-file without losing the work already applied. Unlike STOP,
# which exits, this waits: the process stays alive holding its place, rechecking
# every PAUSE_SLEEP seconds until the file is gone.
check_pause() {
    local where=$1 first=true
    while [ -e "$PAUSE_FILE" ]; do
        if [ "$first" = true ]; then
            log "PAUSED at [$where]: found $PAUSE_FILE."
            log "        Sleeping ${PAUSE_SLEEP}s at a time. Remove the file to resume:  rm $PAUSE_FILE"
            first=false
        else
            log "        still paused at [$where]; sleeping another ${PAUSE_SLEEP}s."
        fi
        sleep "$PAUSE_SLEEP"
    done
    [ "$first" = false ] && log "RESUMED at [$where]: $PAUSE_FILE is gone."
    return 0
}

between_files() {
    local index=$1
    if [ -e "$STOP_FILE" ]; then
        log "STOP: found $STOP_FILE after completing a file; exiting cleanly."
        exit 0
    fi
    check_pause "between files"
    if [ "$index" -lt $(( ${#TARGETS[@]} - 1 )) ] && [ "$INTER_FILE_PAUSE_SECONDS" -gt 0 ]; then
        log "Pausing ${INTER_FILE_PAUSE_SECONDS}s before the next file."
        sleep "$INTER_FILE_PAUSE_SECONDS"
    fi
}

render() { sed "s|{FILE_PATH}|$1|g" "$2"; }
clean_json() { sed -e 's/^```json//' -e 's/^```//' | awk '/^[[:space:]]*\{/{f=1} f'; }

# Non-comment, non-blank lines. Two files identical here differ only in Scaladoc.
code_lines() { grep -vE '^\s*(\*|/\*\*|\*/)' "$1" | grep -v '^\s*$'; }

HOUSE_RULES_FILE="${HOUSE_RULES_FILE:-$TODO_WRITER_DIR/docs/house-rules.md}"
house_rules() {
  if [ -s "$HOUSE_RULES_FILE" ]; then
    echo; echo "=== LEARNED HOUSE RULES (from reviewer feedback on earlier PRs; apply these) ==="
    cat "$HOUSE_RULES_FILE"
  fi
}

# ---- one JSON-mode call ----------------------------------------------------
# $1 provider  $2 model  $3 system prompt file  $4 user payload file  $5 out
#
# Every reviewer and the adjudicator go through here. None of them edits a file:
# the prompts already inline the diff, and they only ever had read tools, so a
# coding agent buys nothing and costs a subscription.
json_call() {
    local prov=$1 model=$2 sysf=$3 usrf=$4 out=$5
    local base key req
    base=$(provider_base "$prov"); key=$(provider_key "$prov")
    req="$WORK_DIR/req.$$.json"
    jq -n --arg m "$model" --arg s "$(cat "$sysf")" --arg u "$(cat "$usrf")" --argjson mt "$MAX_TOKENS" \
      '{model:$m, max_tokens:$mt, response_format:{type:"json_object"},
        messages:[{role:"system",content:$s},{role:"user",content:$u}]}' > "$req"
    # -H User-Agent: Cerebras and OpenRouter sit behind Cloudflare, which rejects
    # some default agents with HTTP 403 "error code: 1010".
    curl -s --max-time 900 "$base/chat/completions" \
        -H "Authorization: Bearer $key" -H "Content-Type: application/json" \
        -H "User-Agent: curl/8.5.0" --data @"$req" \
      | jq -r '.choices[0].message.content // empty' 2>/dev/null | clean_json > "$out"
    rm -f "$req"
    [ -s "$out" ] || echo '{}' > "$out"
}

if [ "$#" -eq 0 ]; then
    echo "Usage: $(basename "$0") <file> [file ...]" >&2; exit 2
fi
TARGETS=("$@")

log "=============================================="
log "fill-doc-todos-free.sh starting  (all roles on free models)"
log "Files: ${#TARGETS[@]} | rounds: $MAX_ROUNDS"
log "  writer      $WRITER_PROVIDER/$WRITER_MODEL"
log "  accuracy    $ACCURACY_PROVIDER/$ACCURACY_MODEL"
log "  style       $STYLE_PROVIDER/$STYLE_MODEL"
log "  adjudicator $ADJUDICATOR_PROVIDER/$ADJUDICATOR_MODEL"
log "PAUSE file: $PAUSE_FILE (recheck every ${PAUSE_SLEEP}s) | STOP file: $STOP_FILE"
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
    log ""
    log "### $REL  ($n_main declarations to document)"

    if [ "$DRY_RUN" = "true" ]; then
        log "DRY RUN: would fill+review $REL"; between_files "$index"; continue
    fi

    ORIG="$WORK_DIR/${SAFE}.orig"; CODE_BEFORE="$WORK_DIR/${SAFE}.code"
    cp -f "$ABS" "$ORIG"; code_lines "$ABS" > "$CODE_BEFORE"

    # Any step that changes a non-comment line gets its file reverted and the
    # file abandoned. Two models silently deleted code while filling comments,
    # and every other signal reported success both times. See RESULTS.md.
    integrity_ok() {
        local stage=$1
        code_lines "$ABS" | diff -q "$CODE_BEFORE" - >/dev/null && return 0
        log "  !! CODE-INTEGRITY FAILURE after $stage on $REL -- reverting and abandoning this file"
        code_lines "$ABS" | diff "$CODE_BEFORE" - | head -20 | while read -r l; do log "  !!   $l"; done
        cp -f "$ORIG" "$ABS"
        return 1
    }

    run_writer() {                       # $1 = prompt file, $2 = log destination
        local key_env
        case "$WRITER_PROVIDER" in
            mistral) key_env=MISTRAL_API_KEY ;; cerebras) key_env=CEREBRAS_API_KEY ;;
            openrouter) key_env=OPENROUTER_API_KEY ;; *) key_env=OPENAI_API_KEY ;;
        esac
        python3 "$DIRECT_WRITER" --file "$ABS" --prompt "$1" \
            --model "$WRITER_MODEL" --base-url "$(provider_base "$WRITER_PROVIDER")" \
            --api-key-env "$key_env" --max-tokens "$MAX_TOKENS" \
            --dump "$REVIEWS_DIR/${SAFE}.reply.txt" > "$2" 2>&1
    }

    check_pause "before writer: $REL"

    # ---- Writer -------------------------------------------------------------
    log "  writer: drafting ($WRITER_PROVIDER/$WRITER_MODEL)..."
    render "$ABS" "$PROMPTS_DIR/doc-writer-prompt-direct.txt" > "$WORK_DIR/${SAFE}.wprompt"
    house_rules >> "$WORK_DIR/${SAFE}.wprompt"
    run_writer "$WORK_DIR/${SAFE}.wprompt" "$REVIEWS_DIR/${SAFE}.writer.log"
    sed 's/^/    /' "$REVIEWS_DIR/${SAFE}.writer.log" | while read -r l; do log "$l"; done
    integrity_ok "writer" || { between_files "$index"; continue; }

    final_acc="$REVIEWS_DIR/${SAFE}.accuracy.json"
    final_sty="$REVIEWS_DIR/${SAFE}.style.json"
    final_adj="$REVIEWS_DIR/${SAFE}.adjudication.json"

    round=1; converged=false; final_refinement=false
    while [ "$round" -le "$MAX_ROUNDS" ]; do
        check_pause "before review round $round: $REL"
        log "  round $round/$MAX_ROUNDS: accuracy ($ACCURACY_MODEL) ‖ style ($STYLE_MODEL)..."

        DIFF_BLOCK="$WORK_DIR/${SAFE}.diff"
        { echo; echo "=== DIFF OF DOCS TO REVIEW (judge only these additions) ==="
          diff -u "$ORIG" "$ABS" || true; } > "$DIFF_BLOCK"

        { render "$ABS" "$PROMPTS_DIR/doc-accuracy-review-prompt.txt"; house_rules
          echo; echo "=== SCHEMA (conform exactly) ==="; cat "$SCHEMA"; } > "$WORK_DIR/${SAFE}.accsys"
        { render "$ABS" "$PROMPTS_DIR/doc-style-review-prompt.txt"; house_rules
          echo; echo "=== SCHEMA (conform exactly) ==="; cat "$SCHEMA"; } > "$WORK_DIR/${SAFE}.stysys"

        json_call "$ACCURACY_PROVIDER" "$ACCURACY_MODEL" "$WORK_DIR/${SAFE}.accsys" "$DIFF_BLOCK" "$final_acc" &
        acc_pid=$!
        json_call "$STYLE_PROVIDER" "$STYLE_MODEL" "$WORK_DIR/${SAFE}.stysys" "$DIFF_BLOCK" "$final_sty" &
        sty_pid=$!
        wait "$acc_pid"; wait "$sty_pid"

        acc_verdict=$(jq -r '.verdict // "revise"' "$final_acc" 2>/dev/null || echo revise)
        sty_verdict=$(jq -r '.verdict // "revise"' "$final_sty" 2>/dev/null || echo revise)
        log "    reviewers: accuracy=$acc_verdict  style=$sty_verdict"

        check_pause "before adjudication round $round: $REL"

        # ---- Adjudicator ----------------------------------------------------
        log "    adjudicating ($ADJUDICATOR_PROVIDER/$ADJUDICATOR_MODEL)..."
        { render "$ABS" "$PROMPTS_DIR/doc-adjudicator-prompt.txt"; house_rules
          echo; echo "=== ADJUDICATION SCHEMA (conform exactly) ==="; cat "$ADJ_SCHEMA"; } > "$WORK_DIR/${SAFE}.adjsys"
        { cat "$DIFF_BLOCK"
          echo; echo "=== ACCURACY REVIEW (JSON) ==="; cat "$final_acc"
          echo; echo "=== STYLE REVIEW (JSON) ==="; cat "$final_sty"; } > "$WORK_DIR/${SAFE}.adjusr"
        json_call "$ADJUDICATOR_PROVIDER" "$ADJUDICATOR_MODEL" \
                  "$WORK_DIR/${SAFE}.adjsys" "$WORK_DIR/${SAFE}.adjusr" "$final_adj"

        adj_verdict=$(jq -r '.verdict // "revise"' "$final_adj" 2>/dev/null || echo revise)
        n_items=$(jq -r '(.resolved_items // []) | length' "$final_adj" 2>/dev/null || echo 0)
        n_dis=$(jq -r '(.disagreements // []) | length' "$final_adj" 2>/dev/null || echo 0)
        log "    adjudicator: $adj_verdict  (${n_items} item(s), ${n_dis} disagreement(s) settled)"

        if [ "$adj_verdict" = "approve" ]; then converged=true; break; fi

        if [ "$round" -eq "$MAX_ROUNDS" ]; then
            final_refinement=true; log "    final refine (not re-reviewed)..."
        else
            log "    refine: working the adjudicated list..."
        fi

        check_pause "before refine round $round: $REL"
        { render "$ABS" "$PROMPTS_DIR/doc-refine-prompt-agent.txt"; house_rules
          echo; cat "$final_adj"; } > "$WORK_DIR/${SAFE}.rprompt"
        run_writer "$WORK_DIR/${SAFE}.rprompt" "$REVIEWS_DIR/${SAFE}.refine${round}.log"
        integrity_ok "refine round $round" || break

        [ "$final_refinement" = "true" ] && break
        round=$((round + 1))
    done

    # ---- Digest -------------------------------------------------------------
    DIGEST="$REVIEWS_DIR/${SAFE}.digest.md"
    {
        echo "# Doc review digest: $REL"; echo
        echo "- models: writer $WRITER_MODEL | accuracy $ACCURACY_MODEL | style $STYLE_MODEL | adjudicator $ADJUDICATOR_MODEL"
        echo "- converged: $converged (up to $MAX_ROUNDS rounds)"
        echo "- final refinement after review limit: $final_refinement (not re-reviewed)"
        echo "- accuracy verdict: $(jq -r '.verdict // "?"' "$final_acc" 2>/dev/null)"
        echo "- style verdict: $(jq -r '.verdict // "?"' "$final_sty" 2>/dev/null)"
        echo "- ADJUDICATOR verdict (final): $(jq -r '.verdict // "?"' "$final_adj" 2>/dev/null)"
        echo; echo "## Reviewer disagreements the adjudicator settled"; echo
        jq -r '(.disagreements // [])[]
          | "- L\(.line) `\(.symbol)` -> ruled for **\(.ruling)**\n  - accuracy: \(.accuracy_position)\n  - style: \(.style_position)\n  - why: \(.rationale)"' \
          "$final_adj" 2>/dev/null || echo "(none / unparseable)"
        echo; echo "## Outstanding worklist at the end"; echo
        jq -r '(.resolved_items // []) | sort_by(.needs_human != true, .severity != "blocker")
          | .[] | "- L\(.line) `\(.symbol)` [\(.severity)/\(.raised_by)\(if .needs_human then "/NEEDS-HUMAN" else "" end)]: \(.instruction)"' \
          "$final_adj" 2>/dev/null || echo "(none / unparseable)"
        echo; echo "## Inline NEEDS-HUMAN markers left in source"
        grep -nE "NEEDS-HUMAN" "$ABS" 2>/dev/null | sed 's/^/- L/' || echo "(none)"
    } > "$DIGEST"

    flagged=$(grep -cE "NEEDS-HUMAN" "$ABS" 2>/dev/null || true)
    remaining=$(grep -c "$MARKER" "$ABS" 2>/dev/null || true)
    log "  done: converged=$converged | NEEDS-HUMAN=${flagged:-0} | unfilled markers left=${remaining:-0}"
    log "  digest: $DIGEST"
    between_files "$index"
done

log ""
log "=============================================="
log "COMPLETE. Reviews + digests in: $REVIEWS_DIR"
log "Changes are in the working tree, UNCOMMITTED."
log "=============================================="
