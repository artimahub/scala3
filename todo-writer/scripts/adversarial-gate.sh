#!/bin/bash

# =============================================================================
# adversarial-gate.sh
#
# One strong model, one pass per file, over the docs the pipeline just wrote.
# Read the whole file and the diff, try to prove the new documentation WRONG,
# and write a markdown report. It changes nothing: the output is for the human
# who is about to open the PR.
#
# Why this exists
# ---------------
# After week 4 was filled and "reviewed", two offline adversarial reviews (one
# by Claude, one by Codex) were run by hand over the finished branch. Between
# them they found the whole Future.never cluster -- 3 outright false statements
# and 15 @return tags contradicting their own method bodies -- plus a doc
# comment attached to the wrong constructor parameter in BatchingExecutor. The
# human reviewer then found essentially the same things.
#
# 50 in-pipeline reviewer calls had found none of it. The difference is not
# subtle and it is not really about model strength: the in-pipeline reviewers
# judge a file that is still being written, one round at a time, and their
# findings get merged, ranked and partly overruled before anything is applied.
# This pass looks at what will actually be committed, once, with no obligation
# to be constructive. That turns out to be the cheapest quality in the pipeline,
# because it is one call per file instead of four per round.
#
# Run it AFTER fill-doc-todos-free.sh and BEFORE committing.
#
# Usage:
#   ./adversarial-gate.sh <file> [file ...]
#   ./adversarial-gate.sh $(git diff --name-only)
#
# Env overrides:
#   GATE_MODEL=anthropic/claude-sonnet-5   GATE_PROVIDER=openrouter
#   GATE_BASE_REF=HEAD          what the docs are diffed against
#   GATE_MAX_TOKENS=32000       GATE_SPACING=10
#
# To gate with an OpenAI model instead:
#   GATE_MODEL=openai/gpt-5.6-terra ./adversarial-gate.sh <files>
# =============================================================================

set -uo pipefail

GATE_MODEL=${GATE_MODEL:-anthropic/claude-sonnet-5}
GATE_PROVIDER=${GATE_PROVIDER:-openrouter}
GATE_BASE_REF=${GATE_BASE_REF:-HEAD}
GATE_MAX_TOKENS=${GATE_MAX_TOKENS:-32000}
GATE_SPACING=${GATE_SPACING:-10}

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TODO_WRITER_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
REPO_ROOT="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
REVIEWS_DIR="$TODO_WRITER_DIR/reviews"
PROMPT="$SCRIPT_DIR/prompts/doc-gate-prompt.txt"
REPEAT_FINDER="$SCRIPT_DIR/repeated-doc-blocks.py"
REPORT="$REVIEWS_DIR/GATE-REPORT.md"
LOG_FILE="$TODO_WRITER_DIR/adversarial-gate.log"

ENV_FILE=${ENV_FILE:-/home/node/.aider/.env}
[ -r "$ENV_FILE" ] && { set -a; . "$ENV_FILE"; set +a; }

case "$GATE_PROVIDER" in
    mistral)    BASE="${MISTRAL_API_BASE:-https://api.mistral.ai/v1}";    KEY="${MISTRAL_API_KEY:-}" ;;
    cerebras)   BASE="${CEREBRAS_API_BASE:-https://api.cerebras.ai/v1}";  KEY="${CEREBRAS_API_KEY:-}" ;;
    openrouter) BASE="${OPENROUTER_API_BASE:-https://openrouter.ai/api/v1}"; KEY="${OPENROUTER_API_KEY:-}" ;;
    poolside)   BASE="${OPENAI_API_BASE:-https://inference.poolside.ai/v1}"; KEY="${OPENAI_API_KEY:-}" ;;
    *) echo "Unknown provider: $GATE_PROVIDER" >&2; exit 2 ;;
esac
[ -n "$KEY" ] || { echo "No API key for provider '$GATE_PROVIDER' (looked in $ENV_FILE)" >&2; exit 2; }
[ -r "$PROMPT" ] || { echo "Missing prompt: $PROMPT" >&2; exit 2; }
for tool in jq curl python3; do
    command -v "$tool" >/dev/null || { echo "Missing required tool: $tool" >&2; exit 2; }
done
[ "$#" -gt 0 ] || { echo "Usage: $(basename "$0") <file> [file ...]" >&2; exit 2; }

mkdir -p "$REVIEWS_DIR"
log() { local m="[$(date '+%H:%M:%S')] $1"; echo "$m"; echo "$m" >> "$LOG_FILE"; }
WORK_DIR="$(mktemp -d)"; trap 'rm -rf "$WORK_DIR"' EXIT

log "=============================================="
log "adversarial-gate.sh: $# file(s) vs $GATE_BASE_REF using $GATE_PROVIDER/$GATE_MODEL"
log "=============================================="

flagged_files=()

for FILE in "$@"; do
    case "$FILE" in /*) ABS="$FILE" ;; *) ABS="$REPO_ROOT/$FILE" ;; esac
    REL="${ABS#"$REPO_ROOT"/}"
    SAFE="$(echo "$REL" | tr '/' '_')"
    [ -r "$ABS" ] || { log "SKIP $REL (unreadable)"; continue; }

    # No diff against the base means nothing was written for this file.
    if git -C "$REPO_ROOT" diff --quiet "$GATE_BASE_REF" -- "$REL" 2>/dev/null; then
        log "SKIP $REL (no changes vs $GATE_BASE_REF)"; continue
    fi

    log "### $REL"
    ORIG="$WORK_DIR/${SAFE}.orig"
    git -C "$REPO_ROOT" show "$GATE_BASE_REF:$REL" > "$ORIG" 2>/dev/null || : > "$ORIG"

    USR="$WORK_DIR/${SAFE}.usr"
    {
        echo "=== FULL SOURCE OF $REL (line-numbered) ==="
        cat -n "$ABS"
        echo
        echo "=== DOC CHANGES UNDER REVIEW (diff vs $GATE_BASE_REF) ==="
        diff -U 15 "$ORIG" "$ABS" || true
        [ -x "$REPEAT_FINDER" ] && python3 "$REPEAT_FINDER" --orig "$ORIG" --new "$ABS" --min 3 2>/dev/null
    } > "$USR"

    SYS="$WORK_DIR/${SAFE}.sys"
    sed "s|{FILE_PATH}|$REL|g" "$PROMPT" > "$SYS"

    REQ="$WORK_DIR/${SAFE}.req"; RAW="$WORK_DIR/${SAFE}.raw"
    jq -n --arg m "$GATE_MODEL" --arg s "$(cat "$SYS")" --arg u "$(cat "$USR")" \
          --argjson mt "$GATE_MAX_TOKENS" \
      '{model:$m, max_tokens:$mt,
        messages:[{role:"system",content:$s},{role:"user",content:$u}]}' > "$REQ"

    OUT="$REVIEWS_DIR/${SAFE}.gate.md"
    delay=30
    for attempt in 1 2 3; do
        curl -s --max-time 900 "$BASE/chat/completions" \
            -H "Authorization: Bearer $KEY" -H "Content-Type: application/json" \
            -H "User-Agent: curl/8.5.0" --data @"$REQ" > "$RAW"
        err=$(jq -r '.error.message // .message // empty' "$RAW" 2>/dev/null)
        if [ -z "$err" ]; then
            jq -r '.choices[0].message.content // empty' "$RAW" > "$OUT" 2>/dev/null
            [ -s "$OUT" ] && break
            err="reply had no parseable content"
        fi
        case "$err" in
            *[Rr]ate*limit*|*429*|*"limit exceeded"*|*"too many"*|*[Qq]uota*|\
            *[Oo]verloaded*|*[Tt]emporar*|*"name resolution"*|*503*)
                log "    rate limited (attempt $attempt/3); waiting ${delay}s"
                sleep "$delay"; delay=$((delay * 2)) ;;
            *)
                log "    !! gate call FAILED: $(echo "$err" | head -c 160)"; break ;;
        esac
    done

    if [ ! -s "$OUT" ]; then
        # An unreachable gate is not a pass. Say so in the file the human reads.
        printf '# Gate report: %s\n\n**THE GATE DID NOT RUN.** No report was produced for this file.\n' "$REL" > "$OUT"
        log "    !! no gate report for $REL"
        flagged_files+=("$REL (gate did not run)")
    else
        # The prompt asks for a FINDINGS line the shell can read without parsing
        # prose. Anything other than a clean "FINDINGS: none" gets the file on
        # the list; when in doubt the human looks.
        if grep -qiE '^FINDINGS:[[:space:]]*none[[:space:]]*$' "$OUT"; then
            log "    clean"
        else
            n=$(grep -ciE '^###[[:space:]]*(B|S|N)[0-9]+' "$OUT" || true)
            log "    ${n:-?} finding(s): $OUT"
            flagged_files+=("$REL (${n:-?} findings)")
        fi
    fi
    [ "$GATE_SPACING" -gt 0 ] && sleep "$GATE_SPACING"
done

{
    echo "# Adversarial gate report"
    echo
    echo "- model: $GATE_PROVIDER/$GATE_MODEL"
    echo "- base: $GATE_BASE_REF"
    echo "- files gated: $#"
    echo
    if [ "${#flagged_files[@]}" -eq 0 ]; then
        echo "No findings. Per-file reports are in \`reviews/*.gate.md\`."
    else
        echo "## Files with findings"
        echo
        for f in "${flagged_files[@]}"; do echo "- $f"; done
        echo
        echo "Read the matching \`reviews/<file>.gate.md\` before committing."
    fi
} > "$REPORT"

log ""
log "=============================================="
if [ "${#flagged_files[@]}" -eq 0 ]; then
    log "GATE CLEAN across $# file(s). Report: $REPORT"
    log "=============================================="
    exit 0
fi
log "GATE FOUND ISSUES in ${#flagged_files[@]} file(s). Report: $REPORT"
for f in "${flagged_files[@]}"; do log "  - $f"; done
log "=============================================="
exit 3
