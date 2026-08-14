#!/bin/bash

# =============================================================================
# fill-doc-todos-free.sh
#
# Every role on a FREE model. No Claude Code, no Codex, no subscription spend.
#
#   Writer      (Mistral  zai-glm-5-2)          drafts
#   repeat up to MAX_ROUNDS:
#       Accuracy review (Mistral  mistral-medium) ┐ sequential, spaced
#       Style review    (Mistral  mistral-large)  ┘
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
#   mistral-large  specific, actionable, properly graded style findings. Chosen
#                  over gpt-oss-120b, which reviews well but was throttled into
#                  148-byte hollow approvals in 3 of 4 rounds: Cerebras limits by
#                  token VOLUME and a review is inherently large, so pacing
#                  cannot fix it. Also beat codestral-latest, which filed ten
#                  identical boilerplate "blockers", and devstral-medium, which
#                  returned an empty approval.
#
# Three families. Reviewer independence matters more than raw reviewer strength:
# two reviewers that share weights agree for the wrong reasons.
#
# Everything is on Mistral now, and EVERY call is sequential and spaced. That is
# a deliberate trade: one provider outage stops the whole pipeline, but Mistral
# limits by request RATE (~1 per 5s), which spacing handles, whereas Cerebras
# limits by token volume, which it cannot. The first run put writer, style and adjudicator all on Mistral; its
# free tier 429'd everything after the writer, losing the style review, the
# adjudication and both refines in under two seconds.
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
#   ACCURACY_MODEL=mistral-medium-latest ACCURACY_PROVIDER=mistral
#   STYLE_MODEL=mistral-large-latest   STYLE_PROVIDER=mistral
#   ADJUDICATOR_MODEL=zai-glm-5-2     ADJUDICATOR_PROVIDER=mistral
#   INTER_FILE_PAUSE_SECONDS=120  PAUSE_SLEEP=30  MAX_TOKENS=32000
#   DRY_RUN=false
# =============================================================================

set -uo pipefail

MAX_ROUNDS=${MAX_ROUNDS:-2}
WRITER_MODEL=${WRITER_MODEL:-zai-glm-5-2};                WRITER_PROVIDER=${WRITER_PROVIDER:-mistral}
ACCURACY_MODEL=${ACCURACY_MODEL:-mistral-medium-latest};   ACCURACY_PROVIDER=${ACCURACY_PROVIDER:-mistral}
STYLE_MODEL=${STYLE_MODEL:-mistral-large-latest};    STYLE_PROVIDER=${STYLE_PROVIDER:-mistral}
ADJUDICATOR_MODEL=${ADJUDICATOR_MODEL:-zai-glm-5-2};      ADJUDICATOR_PROVIDER=${ADJUDICATOR_PROVIDER:-mistral}
MAX_TOKENS=${MAX_TOKENS:-32000}
# Both reviewers get the SAME brief and are both asked to judge accuracy AND
# style. These only tilt the attention. The aim is two full passes over the same
# ground by different models: better that a problem is raised twice than missed
# once.
ACCURACY_EMPHASIS=${ACCURACY_EMPHASIS:-"Your particular focus is FACTUAL CORRECTNESS: API contracts, what the implementation really does, exception and edge-case behaviour, and subtle mismatches between prose and code. Still raise every style problem you see."}
STYLE_EMPHASIS=${STYLE_EMPHASIS:-"Your particular focus is STYLE AND READABILITY: Scaladoc conventions, the project's tag rules, voice and altitude, and whether the text is genuinely useful to an API reader. Still raise every factual error you see, and treat it as a blocker."}
RATE_LIMIT_BACKOFF=${RATE_LIMIT_BACKOFF:-30}   # doubles per retry: 30, 60, 120
PROVIDER_SPACING=${PROVIDER_SPACING:-45}       # gap between same-provider calls
WRITER_MAX_PASSES=${WRITER_MAX_PASSES:-6}     # fill passes before review starts
WRITER_PASS_PAUSE=${WRITER_PASS_PAUSE:-30}    # gap between fill passes
SUSPICIOUS_REVIEW_BYTES=${SUSPICIOUS_REVIEW_BYTES:-400}
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

INTER_FILE_PAUSE_SECONDS=${INTER_FILE_PAUSE_SECONDS:-120}
PAUSE_FILE=${PAUSE_FILE:-"$TODO_WRITER_DIR/PAUSE"}
PAUSE_SLEEP=${PAUSE_SLEEP:-30}
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
#
# PAUSE_SLEEP defaults to 30s because the recheck interval is also the RESUME
# latency -- removing the file does nothing until the next wake-up. 30s suits
# stepping through a run and inspecting each phase. For an unattended overnight
# run where a pause is a genuine hold, raise it: PAUSE_SLEEP=1200.
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
    local base key req raw attempt delay err
    base=$(provider_base "$prov"); key=$(provider_key "$prov")
    # Unique per CALL, not per process. $$ is the script's pid and is identical
    # inside both background subshells, so the two reviewers running in parallel
    # were writing and reading the same request file. They raced, and both came
    # back "no parseable content" -- which looked like a provider failure and was
    # not. Keying off the output path gives one file per role.
    local tag; tag=$(basename "$out" .json)
    req="$WORK_DIR/req.$tag.json"; raw="$WORK_DIR/raw.$tag.json"
    jq -n --arg m "$model" --arg s "$(cat "$sysf")" --arg u "$(cat "$usrf")" --argjson mt "$MAX_TOKENS" \
      '{model:$m, max_tokens:$mt, response_format:{type:"json_object"},
        messages:[{role:"system",content:$s},{role:"user",content:$u}]}' > "$req"

    # Retry on rate limits. Mistral's free tier 429s readily when several roles
    # fire in quick succession, and the first pipeline run lost its style review,
    # its adjudication AND both refines to that -- while reporting a verdict,
    # because an unparseable reply silently became {}. Never again: an error here
    # is logged loudly and the caller can tell a real verdict from a dead call.
    delay=$RATE_LIMIT_BACKOFF
    for attempt in 1 2 3 4; do
        curl -s --max-time 900 "$base/chat/completions" \
            -H "Authorization: Bearer $key" -H "Content-Type: application/json" \
            -H "User-Agent: curl/8.5.0" --data @"$req" > "$raw"
        err=$(jq -r '.error.message // .message // empty' "$raw" 2>/dev/null)
        if [ -z "$err" ]; then
            jq -r '.choices[0].message.content // empty' "$raw" 2>/dev/null | clean_json > "$out"
            if [ -s "$out" ]; then
                # The reply arrived, but is it USABLE? gemma-4-31b once returned
                # 78KB of items as a flat array of loose values instead of
                # objects, truncated mid-string at the token limit. jq could not
                # parse a byte of it, yet `jq -r '.verdict // "revise"'` then
                # produced "revise" -- a fabricated verdict from a broken reply,
                # indistinguishable downstream from a real one. Validate here so
                # that can never happen again.
                if jq empty "$out" 2>/dev/null; then
                    rm -f "$req" "$raw"; return 0
                fi
                log "      !! $prov/$model returned INVALID JSON ($(wc -c < "$out") bytes; likely truncated at the token limit)"
                err="unparseable JSON"
            else
                err="reply had no parseable content"
            fi
        fi
        # Providers word their throttling differently and none of them says
        # "rate limit" reliably. Cerebras returns "Tokens per minute limit
        # exceeded - too many tokens processed", which the original pattern
        # missed entirely, so a plain throttle failed fast instead of backing
        # off. Match the shapes actually observed, plus transient network.
        case "$err" in
            *[Rr]ate*limit*|*429*|*"limit exceeded"*|*"too many"*|*[Qq]uota*|\
            *[Oo]verloaded*|*[Tt]emporar*|*"name resolution"*|*503*)
                log "      rate limited by $prov (attempt $attempt/4); waiting ${delay}s"
                sleep "$delay"; delay=$((delay * 2)) ;;
            *)
                log "      !! $prov/$model call FAILED: $(echo "$err" | head -c 160)"
                break ;;
        esac
    done
    log "      !! $prov/$model produced no usable JSON -- downstream verdicts from this call are NOT real"
    echo '{}' > "$out"
    rm -f "$req" "$raw"
    return 1
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
    render "$ABS" "$PROMPTS_DIR/doc-writer-prompt-direct.txt" > "$WORK_DIR/${SAFE}.wprompt"
    house_rules >> "$WORK_DIR/${SAFE}.wprompt"
    # ---- Writer loop --------------------------------------------------------
    # Fill FIRST, review after. These are separate concerns and want separate
    # stopping rules: "is every placeholder written" and "is the prose good" are
    # not the same question. Governing both with MAX_ROUNDS meant a weak writer
    # pass sent half a file to the reviewers -- one run left 54 of 128 markers,
    # and the style reviewer duly filed a blocker against a `@return TODO FILL
    # IN` that had simply never been written. Reviewing placeholders wastes the
    # reviewers and tells us nothing.
    #
    # The markers are the ledger: whatever a pass misses stays marked, so the
    # next pass picks it up. Paced deliberately -- the backoff exists for
    # surprises, not as a routine throttle. Tripping a provider's limit on every
    # file is a good way to get an account blocked rather than merely slowed.
    markers_before=$(grep -c "$MARKER" "$ABS" 2>/dev/null || true)
    writer_rc=0
    wpass=1
    while [ "$wpass" -le "$WRITER_MAX_PASSES" ]; do
        wbefore=$(grep -c "$MARKER" "$ABS" 2>/dev/null || true)
        [ "${wbefore:-0}" -eq 0 ] && break
        check_pause "before writer pass $wpass: $REL"
        log "  writer pass $wpass/$WRITER_MAX_PASSES ($WRITER_PROVIDER/$WRITER_MODEL): ${wbefore} marker(s) to go..."
        run_writer "$WORK_DIR/${SAFE}.wprompt" "$REVIEWS_DIR/${SAFE}.writer${wpass}.log"
        writer_rc=$?
        sed 's/^/    /' "$REVIEWS_DIR/${SAFE}.writer${wpass}.log" | while read -r l; do log "$l"; done
        integrity_ok "writer pass $wpass" || break
        wafter=$(grep -c "$MARKER" "$ABS" 2>/dev/null || true)
        log "    pass $wpass: ${wbefore} -> ${wafter} markers"
        [ "${wafter:-0}" -eq 0 ] && { log "  all markers filled after $wpass pass(es)"; break; }
        if [ "${wafter:-0}" -eq "${wbefore:-0}" ]; then
            log "  !! writer pass $wpass made no progress; stopping the fill loop with ${wafter} marker(s) left"
            break
        fi
        [ "$wpass" -lt "$WRITER_MAX_PASSES" ] && {
            log "    pausing ${WRITER_PASS_PAUSE}s before the next writer pass"
            sleep "$WRITER_PASS_PAUSE"
        }
        wpass=$((wpass + 1))
    done
    integrity_ok "writer" || { between_files "$index"; continue; }

    # Do NOT review a file the writer did not change. A DNS failure once killed
    # the writer, the script carried on, and BOTH reviewers approved an empty
    # diff -- they were reviewing nothing and rubber-stamping it, which would
    # have been recorded as converged=true with all 128 markers still in place.
    # A stage that fails must not be able to look like a stage that passed.
    markers_after=$(grep -c "$MARKER" "$ABS" 2>/dev/null || true)
    if [ "$writer_rc" -ne 0 ] || [ "${markers_after:-0}" -eq "${markers_before:-0}" ]; then
        log "  !! WRITER PRODUCED NOTHING on $REL (exit $writer_rc, markers ${markers_before:-?} -> ${markers_after:-?})"
        log "  !! skipping review: there is no diff to judge. This file is UNTOUCHED, not done."
        cp -f "$ORIG" "$ABS"
        between_files "$index"; continue
    fi
    log "  writer filled $(( ${markers_before:-0} - ${markers_after:-0} )) marker(s); ${markers_after:-0} left"

    final_acc="$REVIEWS_DIR/${SAFE}.accuracy.json"
    final_sty="$REVIEWS_DIR/${SAFE}.style.json"
    final_adj="$REVIEWS_DIR/${SAFE}.adjudication.json"

    round=1; converged=false; final_refinement=false
    while [ "$round" -le "$MAX_ROUNDS" ]; do
        check_pause "before review round $round: $REL"
        log "  round $round/$MAX_ROUNDS: accuracy ($ACCURACY_MODEL) then style ($STYLE_MODEL)..."

        DIFF_BLOCK="$WORK_DIR/${SAFE}.diff"
        { echo; echo "=== DIFF OF DOCS TO REVIEW (judge only these additions) ==="
          diff -u "$ORIG" "$ABS" || true; } > "$DIFF_BLOCK"

        # One shared brief, rendered twice with different emphasis.
        render_review() {   # $1 = emphasis text, $2 = destination
            render "$ABS" "$PROMPTS_DIR/doc-review-prompt.txt" \
              | awk -v e="$1" '{gsub(/\{EMPHASIS\}/, e); print}'
            house_rules
            echo; echo "=== SCHEMA (conform exactly) ==="; cat "$SCHEMA"
        }
        render_review "$ACCURACY_EMPHASIS" > "$WORK_DIR/${SAFE}.accsys"
        render_review "$STYLE_EMPHASIS"    > "$WORK_DIR/${SAFE}.stysys"

        # The accuracy reviewer may share a provider with the writer; space it.
        if [ "$ACCURACY_PROVIDER" = "$WRITER_PROVIDER" ] && [ "$PROVIDER_SPACING" -gt 0 ]; then
            sleep "$PROVIDER_SPACING"
        fi

        # Sequential, not parallel. There is no hurry, and running them
        # concurrently bought nothing but risk: the two calls raced over a shared
        # temp file, and firing two requests at one provider within a second is
        # exactly what free-tier limits punish. Reviewer independence comes from
        # different models, not from concurrency.
        json_call "$ACCURACY_PROVIDER" "$ACCURACY_MODEL" "$WORK_DIR/${SAFE}.accsys" "$DIFF_BLOCK" "$final_acc"
        if [ "$STYLE_PROVIDER" = "$ACCURACY_PROVIDER" ] && [ "$PROVIDER_SPACING" -gt 0 ]; then
            sleep "$PROVIDER_SPACING"
        fi
        json_call "$STYLE_PROVIDER" "$STYLE_MODEL" "$WORK_DIR/${SAFE}.stysys" "$DIFF_BLOCK" "$final_sty"

        # A review that did not parse is UNAVAILABLE, not "revise". Collapsing
        # the two hid a dead reviewer behind a plausible verdict.
        verdict_of() {
            jq -e 'has("verdict")' "$1" >/dev/null 2>&1 || { echo "UNAVAILABLE"; return; }
            jq -r '.verdict' "$1" 2>/dev/null
        }
        # A THROTTLED reviewer is the nastiest failure here, because a
        # truncated reply can still be valid JSON. gpt-oss-120b returned 152
        # bytes of {"verdict":"approve","items":[]} while Cerebras was refusing
        # it on tokens-per-minute -- indistinguishable from a genuine clean bill
        # of health, and the adjudicator converged on it. The same model given a
        # clear window returned real blockers. Size is the tell.
        flag_suspicious() {   # $1 = json, $2 = label
            local sz v n
            sz=$(wc -c < "$1" 2>/dev/null || echo 0)
            v=$(jq -r '.verdict // ""' "$1" 2>/dev/null)
            n=$(jq -r '(.items // []) | length' "$1" 2>/dev/null)
            if [ "$v" = approve ] && [ "${n:-0}" -eq 0 ] && [ "${sz:-0}" -lt "$SUSPICIOUS_REVIEW_BYTES" ]; then
                log "      ?? $2 approved with 0 items in only ${sz} bytes -- suspiciously terse."
                log "      ?? A throttled or truncated reply can still parse as a clean approval. Treat with doubt."
            fi
        }
        flag_suspicious "$final_acc" "$ACCURACY_PROVIDER/$ACCURACY_MODEL"
        flag_suspicious "$final_sty" "$STYLE_PROVIDER/$STYLE_MODEL"
        acc_verdict=$(verdict_of "$final_acc")
        sty_verdict=$(verdict_of "$final_sty")
        log "    reviewers: accuracy=$acc_verdict  style=$sty_verdict"
        if [ "$acc_verdict" = UNAVAILABLE ] && [ "$sty_verdict" = UNAVAILABLE ]; then
            log "    !! BOTH reviews unavailable -- nothing to adjudicate; treating this round as unreviewed"
        fi

        check_pause "before adjudication round $round: $REL"

        # Space this away from the writer call on the same provider. The reviews
        # above take a few seconds; on a free tier that is not enough headroom.
        if [ "$ADJUDICATOR_PROVIDER" = "$WRITER_PROVIDER" ] && [ "$PROVIDER_SPACING" -gt 0 ]; then
            sleep "$PROVIDER_SPACING"
        fi

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
