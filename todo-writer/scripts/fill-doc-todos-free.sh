#!/bin/bash

# =============================================================================
# fill-doc-todos-free.sh
#
# Free API models everywhere EXCEPT the accuracy reviewer, which runs on the
# local `claude` CLI against the Claude subscription signed in on this machine.
# See "WEEK 4 POST-MORTEM" below for why that one role gets the good model.
#
#   Writer      (Mistral    devstral-latest)      drafts
#   repeat up to MAX_ROUNDS:
#       Accuracy review (claude CLI  sonnet)             ┐ sequential, spaced
#       Style review    (Mistral     mistral-large-latest) ┘
#       Adjudicator     (Mistral  devstral-latest) merges both into ONE verdict
#       if adjudicator approves -> done
#       else Writer refines against the adjudicated worklist
#   after the final refine: one VERIFICATION review (no further refine), so the
#   last edit made to a file is never the unreviewed one
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
# Transports: the writer uses direct-writer.py, the style reviewer and the
# adjudicator are plain JSON-mode HTTP calls, and the accuracy reviewer runs
# through the `claude` CLI in headless print mode (`-p --output-format json`),
# reading `.result` out of the envelope. Of the four writer clients tried
# (aider, Codex CLI, pool, direct), only `direct` worked for every model; aider
# alone broke three of four. See RESULTS.md.
#
# The CLI is deliberately confined to Read, Grep and Glob. It is reviewing, and
# a reviewer that can edit is a second writer with no integrity check behind it.
#
# -----------------------------------------------------------------------------
# WEEK 4 POST-MORTEM (util + concurrent, PR #75). Read before changing any of
# the integrity rules below; each one is here because week 4 shipped without it.
#
#   * The accuracy reviewer was dead for the whole partition. mistral-medium-2508
#     returned {"verdict":"approve","items":[]} in 129-166 bytes, 42 times in a
#     row: 19 of the 23 files got a zero-item accuracy review. The script noticed
#     every one of them and only LOGGED a warning, so a dead reviewer was
#     indistinguishable from a clean bill of health. The human reviewer then
#     filed 51 comments, ~46 of them factual errors -- exactly the class the
#     accuracy reviewer exists to catch. A hollow approval is now retried once
#     and then treated as NO REVIEW AT ALL (see graded_review).
#
#   * The reviewers never saw the code. They got `diff -u` (3 lines of context)
#     plus a prompt telling them to "read the file at <path>", which a plain
#     chat completion cannot do. Every error the human found required reading a
#     body: `= this` in Future.never, `catch { case NonFatal(e) => fa(e) }` in
#     Success.fold. The source now goes into the review payload (see
#     build_review_input).
#
#   * The last edit to a file was never reviewed. 23 of 26 files ended on
#     "final refine (not re-reviewed)". FINAL_VERIFY closes that.
#
#   * One templated mistake became 20 review comments. DurationConversions got
#     `@param c the classifier instance` (wrong; `ev` is the classifier) copied
#     across 20 near-identical methods. Repeated blocks are now grouped and
#     judged once, with the ruling applied to the whole group.
#
#   * docs/house-rules.md was still empty after four PRs, so four rounds of human
#     feedback taught the pipeline nothing. It is seeded now; keep appending.
# -----------------------------------------------------------------------------
#
# This script does NOT commit. It edits the working tree.
#
# Exit status: 0 when every file was filled AND reviewed; 3 when at least one
# file ended NOT REVIEWED (its name is in reviews/NOT-REVIEWED.txt). A nonzero
# exit is not a crash: the docs are in the tree, but do not put an unreviewed
# file in a PR without reading it yourself.
#
# Usage:
#   ./fill-doc-todos-free.sh <file> [file ...]
#
# Control files (create/remove while it runs). All three are honoured at every
# phase boundary AND during the sleeps between them, within STOP_POLL seconds:
#
#   PAUSE    -> todo-writer/PAUSE
#               holds at the next phase boundary and waits, rechecking every
#               PAUSE_SLEEP seconds, until you remove the file
#   STOP     -> todo-writer/stop-fill-doc-todos
#               finishes the file in hand, then exits cleanly. Nothing is left
#               half-done, but on a big file "the file in hand" can be an hour
#   STOP NOW -> todo-writer/stop-fill-doc-todos-now
#               exits within seconds, REVERTING the file it interrupts. That
#               revert is the point: a file left filled-but-unreviewed has no
#               markers, so every later run would skip it as already done
#
# Env overrides:
#   MAX_ROUNDS=3
#   WRITER_MODEL=devstral-latest        WRITER_PROVIDER=mistral
#   ACCURACY_MODEL=sonnet               ACCURACY_PROVIDER=claude-cli
#   STYLE_MODEL=mistral-large-latest    STYLE_PROVIDER=mistral
#   ADJUDICATOR_MODEL=devstral-latest   ADJUDICATOR_PROVIDER=mistral
#   INTER_FILE_PAUSE_SECONDS=120  PAUSE_SLEEP=30  MAX_TOKENS=32000
#   WRITER_MAX_PASSES=6  WRITER_PASS_PAUSE=60  PROVIDER_SPACING=45
#   REVIEW_FILE_MAX_LINES=1500  REVIEW_DIFF_CONTEXT=25  REPEAT_GROUP_MIN=3
#   FINAL_VERIFY=true  SUSPICIOUS_REVIEW_BYTES=400  SUSPICIOUS_RETRY_PAUSE=45
#   CLI_TIMEOUT=1800  STOP_POLL=5  DRY_RUN=false
#
# PROVIDERS. Any role takes any provider; they come in two kinds.
#   HTTP + API key:  mistral, cerebras, openrouter, poolside
#   local CLI:       claude-cli, codex-cli
# A CLI role costs subscription time instead of API credit, and gets Read, Grep
# and Glob over the repo -- so a reviewer can open the file an override or an
# implicit comes from, which no HTTP role can do at any price. Models are named
# the way that CLI names them (`sonnet`, `opus`; `gpt-5.6-terra` for codex).
#
#   ACCURACY_PROVIDER=codex-cli ACCURACY_MODEL=gpt-5.6-terra ./fill-doc-todos-free.sh ...
#
# To go back to an all-free run (and accept week 4's failure mode):
#   ACCURACY_PROVIDER=mistral ACCURACY_MODEL=mistral-medium-latest ./fill-doc-todos-free.sh ...
# =============================================================================

set -uo pipefail

# 3 for week 5 (math + coll-generic). Week 4 ran 2 and converged 3 times in 26
# files, but its round 1 was worthless: the accuracy reviewer was dead, so both
# rounds were the style reviewer relitigating wording. With a real accuracy
# review in round 1, the extra round is a real second chance at a real finding.
# Each round is 3 calls (accuracy, style, adjudicator) plus a refine, and a
# non-converging file now costs MAX_ROUNDS+1 accuracy calls counting the
# verification pass.
MAX_ROUNDS=${MAX_ROUNDS:-3}
# Every role on a local coding-agent CLI, billed to the subscriptions signed in
# on this machine. This is where the pipeline started, before the all-free
# experiment; it comes back because Mistral's ceiling turned out to be the
# binding constraint, not its quality. Eight of the nine dead calls across six
# weeks were Mistral, all of them oversized payloads: 128 KB of diff on
# FunctionWrappers killed two roles at once and cost that file every refine.
#
# Codex on accuracy is deliberate. `codex exec --output-schema` enforces the
# JSON shape at the tool layer, which no other transport here can do -- `claude
# -p` has no equivalent, so its schema is piped into the prompt and honoured on
# trust. That trust broke once already, on TrieMap, where a 26-turn review came
# back as prose and was discarded. And it keeps the two reviewers in different
# model families, which is where reviewer independence actually comes from.
WRITER_MODEL=${WRITER_MODEL:-opus};                        WRITER_PROVIDER=${WRITER_PROVIDER:-claude-cli}
# The one role that is not on a free API model. Week 4 proved that a weak or
# throttled accuracy reviewer is worse than none: it produces a verdict that
# LOOKS like review and stops anyone looking further.
#
# It runs through the local `claude` CLI, so the work is billed to the Claude
# subscription already signed in on this machine rather than to a per-token API
# key. That also buys something no API-key route can: with Read/Grep/Glob the
# reviewer can open the OTHER files a declaration depends on. Reviewing
# `Ordering.scala` means following `Numeric`; reviewing an override means
# reading the member it overrides.
ACCURACY_MODEL=${ACCURACY_MODEL:-gpt-5.6-terra}; ACCURACY_PROVIDER=${ACCURACY_PROVIDER:-codex-cli}
STYLE_MODEL=${STYLE_MODEL:-sonnet};         STYLE_PROVIDER=${STYLE_PROVIDER:-claude-cli}
ADJUDICATOR_MODEL=${ADJUDICATOR_MODEL:-sonnet};   ADJUDICATOR_PROVIDER=${ADJUDICATOR_PROVIDER:-claude-cli}
MAX_TOKENS=${MAX_TOKENS:-32000}
# ONE brief, rendered identically for both reviewers. There is no "accuracy
# reviewer" and no "style reviewer" any more: each is asked to judge accuracy
# AND style in full, and the two differ only in which model runs them and what
# each can see.
#
# The split used to be a pair of emphasis paragraphs tilting one toward facts
# and one toward prose. It read well and it cost coverage: a reviewer told its
# particular focus is style is a reviewer with an excuse not to read the body.
# Week 4 is the cautionary case, where the factual reviewer was dead and the
# stylistic one filed 45 rounds of wording notes without ever noticing that
# Future.never's `map` documented a function it does not apply.
#
# Reviewer independence is supposed to come from different models looking at the
# same thing, not from dividing the job between them.
REVIEW_EMPHASIS=${REVIEW_EMPHASIS:-"Judge ACCURACY and STYLE equally, and in full. Neither is another reviewer's department: a factual error is a blocker whoever finds it, and a broken convention is a finding whoever finds it. Read the implementation of every declaration you assess before you judge its prose."}
RATE_LIMIT_BACKOFF=${RATE_LIMIT_BACKOFF:-30}   # doubles per retry: 30, 60, 120
PROVIDER_SPACING=${PROVIDER_SPACING:-45}       # gap between same-provider calls
WRITER_MAX_PASSES=${WRITER_MAX_PASSES:-6}     # fill passes before review starts
WRITER_PASS_PAUSE=${WRITER_PASS_PAUSE:-60}    # gap between fill passes
SUSPICIOUS_REVIEW_BYTES=${SUSPICIOUS_REVIEW_BYTES:-400}
SUSPICIOUS_RETRY_PAUSE=${SUSPICIOUS_RETRY_PAUSE:-45}  # wait before the one retry
# Reviewers get the source, not just the diff. Whole file when it fits, so the
# reviewer can follow an override to the member it overrides; a wide diff
# otherwise.
#
# Measured against the file WITH its new documentation in it, which is the thing
# actually being sent -- and that is the trap this number has to clear. Week 5
# set it to 1500 after checking that all 23 files in the partition were under
# it. TrieMap.scala is 1241 lines in the repo; with 99 doc comments added it
# became 1639, crossed the line, and all four of its reviews silently fell back
# to diff-only. The hardest file in the partition, a lock-free concurrent map,
# got the least context, which is exactly backwards.
#
# 3000 lines is roughly 40k tokens of Scala. Every current reviewer handles it,
# and it leaves room for documentation to inflate a file by half its length.
REVIEW_FILE_MAX_LINES=${REVIEW_FILE_MAX_LINES:-3000}
REVIEW_DIFF_CONTEXT=${REVIEW_DIFF_CONTEXT:-25}
REPEAT_GROUP_MIN=${REPEAT_GROUP_MIN:-3}   # identical doc blocks before grouping
FINAL_VERIFY=${FINAL_VERIFY:-true}        # review the final refine, do not ship it blind
CLI_TIMEOUT=${CLI_TIMEOUT:-1800}          # per call, for claude-cli / codex-cli
# How a CLI writer works.
#   agent  -- it holds Edit and rewrites the file itself. What the original
#             subscription pipeline did. Fewer moving parts, and the model
#             places its own text rather than describing an edit for a parser.
#   blocks -- it emits ID / SEARCH-REPLACE blocks and direct-writer.py applies
#             them, refusing any block that touches a non-comment line.
#
# agent trades a per-edit guard for a per-file one. integrity_ok still compares
# every non-comment, non-blank line before and after and reverts the file if a
# single one moved, so code cannot survive being mangled; it just gets caught
# after the fact rather than refused up front. Two models did silently delete
# code under the old agent pipeline (see RESULTS.md), which is why that check
# exists and why it runs after every writer and refine call.
WRITER_MODE=${WRITER_MODE:-agent}
WRITER_TOOLS=${WRITER_TOOLS:-Edit,Read,Grep,Glob}
REVIEWER_TOOLS=${REVIEWER_TOOLS:-Read,Grep,Glob}   # no Edit: reviewers judge, they do not fix
# Mistral answers a payload over roughly 125 KB with an empty body, every time.
# Retrying that is not resilience, it is four sleeps into a wall: week 6's
# FunctionWrappers.scala burned 15 minutes per round on two roles doing exactly
# that, and produced no adjudication and therefore no refine on any round.
# Below these sizes, behave as before. Above them, adapt instead of retrying.
MISTRAL_PAYLOAD_LIMIT_KB=${MISTRAL_PAYLOAD_LIMIT_KB:-100}
DRY_RUN=${DRY_RUN:-false}
MARKER="TODO FILL IN"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TODO_WRITER_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
REPO_ROOT="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
PROMPTS_DIR="$SCRIPT_DIR/prompts"
SCHEMA="$SCRIPT_DIR/schemas/doc-review.schema.json"
ADJ_SCHEMA="$SCRIPT_DIR/schemas/doc-adjudication.schema.json"
DIRECT_WRITER="$TODO_WRITER_DIR/experiments/direct-writer.py"
REPEAT_FINDER="$SCRIPT_DIR/repeated-doc-blocks.py"
REVIEWS_DIR="$TODO_WRITER_DIR/reviews"
LOG_FILE="$TODO_WRITER_DIR/fill-doc-todos-free.log"
NOT_REVIEWED_FILE="$REVIEWS_DIR/NOT-REVIEWED.txt"

INTER_FILE_PAUSE_SECONDS=${INTER_FILE_PAUSE_SECONDS:-120}
PAUSE_FILE=${PAUSE_FILE:-"$TODO_WRITER_DIR/PAUSE"}
PAUSE_SLEEP=${PAUSE_SLEEP:-30}
STOP_FILE=${STOP_FILE:-"$TODO_WRITER_DIR/stop-fill-doc-todos"}
STOP_NOW_FILE=${STOP_NOW_FILE:-"$TODO_WRITER_DIR/stop-fill-doc-todos-now"}
STOP_POLL=${STOP_POLL:-5}     # how long a sleep can ignore a stop request
WORK_DIR="$(mktemp -d)"
trap 'rm -rf "$WORK_DIR"' EXIT

mkdir -p "$REVIEWS_DIR"

# ---- credentials -----------------------------------------------------------
ENV_FILE=${ENV_FILE:-/home/node/.aider/.env}
[ -r "$ENV_FILE" ] && { set -a; . "$ENV_FILE"; set +a; }

# Two kinds of provider. Most are an OpenAI-compatible HTTP endpoint plus an API
# key. `claude-cli` and `codex-cli` are the coding-agent binaries installed on
# this machine, driven in headless print mode: no key, billed to whatever
# subscription is signed in, and -- the part that matters for a reviewer -- able
# to open files the prompt did not include.
is_cli_provider() { case "$1" in claude-cli|codex-cli) return 0 ;; *) return 1 ;; esac; }
cli_binary()      { echo "${1%-cli}"; }

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
    if is_cli_provider "$prov"; then
        bin=$(cli_binary "$prov")
        command -v "$bin" >/dev/null || {
            echo "Provider '$prov' needs the '$bin' CLI on PATH." >&2; exit 2; }
        continue
    fi
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
# ---- STOP, in two flavours -------------------------------------------------
# STOP_FILE is graceful: finish the file in hand, then exit. It is the right
# default, because the alternative is leaving a file half-processed.
#
# STOP_NOW_FILE is immediate: abandon the current file and exit within seconds.
# It exists because "graceful" can mean a very long wait -- TrieMap.scala held
# the pipeline for an hour, and a stop requested at minute two would not have
# been honoured until minute sixty.
#
# An immediate stop REVERTS the file it interrupts, and that is the whole point
# rather than a nicety. A killed run left DefaultSerializationProxy.scala fully
# filled but never reviewed; since the resume logic skips any file with no
# markers left, that file would have been silently skipped forever after and
# shipped with no review at all. Reverting puts its markers back, so the next
# run picks it up as unfinished work, which is what it is.
FILE_IN_PROGRESS=false      # true between the ORIG snapshot and the digest

check_stop_now() {
    local where=$1
    [ -e "$STOP_NOW_FILE" ] || return 0
    log "STOP NOW at [$where]: found $STOP_NOW_FILE."
    if [ "$FILE_IN_PROGRESS" = true ] && [ -n "${ORIG:-}" ] && [ -r "${ORIG:-}" ]; then
        cp -f "$ORIG" "$ABS"
        log "        reverted $REL to its pre-writer state; it is unfinished, not done."
        log "        (leaving it filled but unreviewed would let the next run skip it)"
    fi
    log "        exiting now. Remove the file before restarting:  rm $STOP_NOW_FILE"
    exit 0
}

# Graceful stop, honoured only where stopping is safe: between files.
check_stop_graceful() {
    local where=$1
    [ -e "$STOP_FILE" ] || return 0
    log "STOP at [$where]: found $STOP_FILE; exiting cleanly at a file boundary."
    exit 0
}

# Interruptible sleep. A pause that cannot be interrupted is a pause that eats
# your stop request: the old inter-file sleep checked STOP before it slept and
# not after, so a stop touched during those 120 seconds did nothing until the
# NEXT file had also finished.
#
# $1 seconds  $2 where  $3 "boundary" if a graceful stop may exit here
sleep_interruptible() {
    local total=$1 where=$2 kind=${3:-} slept=0 slice
    while [ "$slept" -lt "$total" ]; do
        check_stop_now "$where"
        [ "$kind" = boundary ] && check_stop_graceful "$where"
        slice=$(( total - slept )); [ "$slice" -gt "$STOP_POLL" ] && slice=$STOP_POLL
        sleep "$slice"
        slept=$(( slept + slice ))
    done
    check_stop_now "$where"
    [ "$kind" = boundary ] && check_stop_graceful "$where"
    return 0
}

check_pause() {
    local where=$1 first=true
    # Every existing check_pause call site becomes an immediate-stop checkpoint
    # too, which is what puts STOP NOW at all seven phase boundaries -- before
    # the writer, each writer pass, each review round, adjudication, each refine,
    # the verification review, and between files -- instead of one.
    check_stop_now "$where"
    while [ -e "$PAUSE_FILE" ]; do
        check_stop_now "$where"
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

# $2 = "nosleep" to skip the inter-file pause. The pause exists to space out API
# calls; a file that was skipped made none, so waiting after it buys nothing. On
# a resumed run most files are already done, and pausing after each would add
# INTER_FILE_PAUSE_SECONDS x (files already complete) of dead time -- 26 minutes
# on a partition that is 13 files in. STOP and PAUSE are still honoured either
# way, so control does not depend on whether work happened.
between_files() {
    local index=$1 mode=${2:-}
    check_stop_now "between files"
    check_stop_graceful "between files"
    check_pause "between files"
    [ "$mode" = nosleep ] && return 0
    if [ "$index" -lt $(( ${#TARGETS[@]} - 1 )) ] && [ "$INTER_FILE_PAUSE_SECONDS" -gt 0 ]; then
        log "Pausing ${INTER_FILE_PAUSE_SECONDS}s before the next file (stop/pause honoured within ${STOP_POLL}s)."
        sleep_interruptible "$INTER_FILE_PAUSE_SECONDS" "between files" boundary
    fi
    # Checked AFTER the pause as well as before it. A stop or pause touched
    # while the pipeline was sleeping used to be invisible until another whole
    # file had been processed.
    check_stop_graceful "after the inter-file pause"
    check_pause "after the inter-file pause"
}

render() { sed "s|{FILE_PATH}|$1|g" "$2"; }
clean_json() { sed -e 's/^```json//' -e 's/^```//' | awk '/^[[:space:]]*\{/{f=1} f'; }

# Non-comment, non-blank lines. Two files identical here differ only in Scaladoc.
code_lines() { grep -vE '^\s*(\*|/\*\*|\*/)' "$1" | grep -v '^\s*$'; }

# What a role may open depends on its transport, and the prompt must say which.
# An HTTP reviewer told to "read the file" cannot, and invents; a CLI reviewer
# told it has no tools will not look, which wastes the one advantage it has.
tools_note() {   # $1 = provider
    echo
    echo "=== WHAT YOU CAN OPEN ==="
    if is_cli_provider "$1"; then
        cat <<'EOT'
You have Read, Grep and Glob over this repository. When the source below does
not settle a question -- an overridden member defined elsewhere, a type from
another file, a helper the body calls -- open the file and read it. Prefer
looking to guessing; that is why you have the tools.

Everything you assert must still be supported by text you actually read, here or
in a file you opened. Do not edit anything: you are reviewing, and a reviewer
that writes is just an unreviewed second writer.

However many turns you spend looking, your FINAL message must be the JSON object
and nothing else. Finishing a long investigation with a prose write-up is the
one way to make all of that work count for nothing: the JSON is what is read,
and prose is discarded unread.
EOT
    else
        cat <<'EOT'
You have no tools and cannot open anything else. Everything you assert must be
supported by text visible in this prompt. If the answer depends on a file you
were not given, say so in the item and set confidence "low" rather than
guessing.
EOT
    fi
}

# Roles no longer get identical payloads, so each must be told what it has.
# A reviewer that believes it was given the source will reason about code it
# cannot see, which is how week 4's reviewers invented context.
payload_note() {   # $1 = "source" | "diff"
    echo
    echo "=== WHAT YOU ARE GIVEN ==="
    if [ "$1" = source ]; then
        cat <<'EOT'
  1. the source of the file, line-numbered. It is the truth about this file and
     outranks anything the documentation claims.
  2. a unified diff of ONLY the Scaladoc that was just written or changed.
  3. sometimes, an index of doc blocks the writer generated more than once.
EOT
    else
        cat <<'EOT'
  1. a unified diff of ONLY the Scaladoc that was just written or changed, with
     several lines of surrounding context.
  2. sometimes, an index of doc blocks the writer generated more than once.

You do NOT have the whole source file. Judge what the diff shows you. Where a
finding would depend on code you cannot see -- what a body really does, what an
override overrides -- either say so and set confidence "low", or leave it to the
other reviewer, who does have the source. Do not reason about code from a
signature and call the result a fact.
EOT
    fi
}

HOUSE_RULES_FILE="${HOUSE_RULES_FILE:-$TODO_WRITER_DIR/docs/house-rules.md}"
house_rules() {
  if [ -s "$HOUSE_RULES_FILE" ]; then
    echo; echo "=== LEARNED HOUSE RULES (from reviewer feedback on earlier PRs; apply these) ==="
    cat "$HOUSE_RULES_FILE"
  fi
}

# ---- one call through a local coding-agent CLI -----------------------------
# Same contract as json_call: validated JSON in $out and return 0, or '{}' and
# return 1. Only the transport differs.
#
# The CLI is given Read, Grep and Glob and nothing else. A reviewer that can
# open the file it is reviewing is worth more than any amount of context
# stuffing, and a reviewer that could EDIT would quietly become a second writer
# with no integrity check behind it.
#
# Note what is NOT here: max_tokens, response_format, temperature. The CLI owns
# those. The schema still reaches the model, because the caller appends it to
# the system prompt, which is also how the old subscription pipeline did it.
cli_call() {
    local prov=$1 model=$2 sysf=$3 usrf=$4 out=$5
    local bin tag raw errf attempt delay err rc cost
    bin=$(cli_binary "$prov")
    tag=$(basename "$out" .json)
    raw="$WORK_DIR/raw.$tag.json"; errf="$WORK_DIR/err.$tag.txt"
    delay=$RATE_LIMIT_BACKOFF

    for attempt in 1 2 3 4; do
        rc=0; err=""
        : > "$raw"; : > "$errf"
        case "$prov" in
            claude-cli)
                # Run from the repo root so a relative path in the prompt
                # resolves, and so Read/Grep/Glob are rooted at the source tree.
                cat "$sysf" "$usrf" \
                  | ( cd "$REPO_ROOT" && timeout "$CLI_TIMEOUT" "$bin" \
                        --dangerously-skip-permissions -p --model "$model" \
                        --allowedTools "$REVIEWER_TOOLS" --output-format json ) \
                    > "$raw" 2> "$errf" || rc=$?
                if [ "$rc" -eq 124 ]; then
                    err="claude CLI timed out after ${CLI_TIMEOUT}s"
                elif [ ! -s "$raw" ]; then
                    err="claude CLI produced no output (rc=$rc): $(head -c 200 "$errf")"
                elif [ "$(jq -r '.is_error // false' "$raw" 2>/dev/null)" = "true" ]; then
                    err="claude CLI reported an error: $(jq -r '.result // ""' "$raw" | head -c 200)"
                else
                    jq -r '.result // empty' "$raw" 2>/dev/null | clean_json > "$out"
                    cost=$(jq -r '.total_cost_usd // empty' "$raw" 2>/dev/null)
                    [ -n "$cost" ] && log "      ($prov/$model: \$$cost against the subscription)"
                fi
                ;;
            codex-cli)
                # codex writes its last message straight to a file, so there is
                # no envelope to unwrap.
                cat "$sysf" "$usrf" \
                  | timeout "$CLI_TIMEOUT" "$bin" exec --model "$model" -s read-only \
                        --skip-git-repo-check -C "$REPO_ROOT" \
                        --output-schema "$SCHEMA" --output-last-message "$WORK_DIR/last.$tag" - \
                    > "$errf" 2>&1 || rc=$?
                if [ "$rc" -eq 124 ]; then
                    err="codex CLI timed out after ${CLI_TIMEOUT}s"
                elif [ ! -s "$WORK_DIR/last.$tag" ]; then
                    err="codex CLI produced no last message (rc=$rc): $(tail -c 200 "$errf")"
                else
                    clean_json < "$WORK_DIR/last.$tag" > "$out"
                fi
                ;;
        esac

        if [ -z "$err" ]; then
            if [ -s "$out" ] && jq empty "$out" 2>/dev/null; then return 0; fi
            err="reply was not usable JSON ($(wc -c < "$out" 2>/dev/null || echo 0) bytes)"
        fi

        # Same shapes as the HTTP path, plus the ones a subscription CLI uses
        # when the plan's window is exhausted. That is a wait, not a failure.
        case "$err" in
            # A coding-agent CLI does not always end where you asked it to. On
            # TrieMap.scala the reviewer worked for 26 turns, found four real
            # blockers, and then wrote them up as PROSE -- a perfectly good
            # review that no downstream step could read. That is worth another
            # try, not a dead call, so it retries alongside the throttles.
            *"not usable JSON"*|\
            *[Rr]ate*limit*|*429*|*"limit exceeded"*|*"usage limit"*|*[Qq]uota*|\
            *[Oo]verloaded*|*[Tt]emporar*|*"timed out"*|*503*)
                log "      $prov throttled, slow, or off-format (attempt $attempt/4): $(echo "$err" | head -c 120)"
                log "      waiting ${delay}s"
                sleep "$delay"; delay=$((delay * 2)) ;;
            *)
                log "      !! $prov/$model call FAILED: $(echo "$err" | head -c 200)"
                break ;;
        esac
    done
    log "      !! $prov/$model produced no usable JSON -- downstream verdicts from this call are NOT real"
    echo '{}' > "$out"
    return 1
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
    if is_cli_provider "$prov"; then cli_call "$prov" "$model" "$sysf" "$usrf" "$out"; return $?; fi
    base=$(provider_base "$prov"); key=$(provider_key "$prov")
    # Unique per CALL, not per process. $$ is the script's pid and is identical
    # inside both background subshells, so the two reviewers running in parallel
    # were writing and reading the same request file. They raced, and both came
    # back "no parseable content" -- which looked like a provider failure and was
    # not. Keying off the output path gives one file per role.
    local tag; tag=$(basename "$out" .json)
    req="$WORK_DIR/req.$tag.json"; raw="$WORK_DIR/raw.$tag.json"

    # JSON mode is requested but not required. Not every provider/model pair
    # accepts response_format, and a 400 over the response FORMAT would lose a
    # review whose CONTENT was fine. build_request re-runs without it if the
    # provider objects; the prompts carry the schema and clean_json strips code
    # fences, so a plain reply still parses.
    local json_mode=true
    build_request() {
        if [ "$json_mode" = true ]; then
            jq -n --arg m "$model" --arg s "$(cat "$sysf")" --arg u "$(cat "$usrf")" --argjson mt "$MAX_TOKENS" \
              '{model:$m, max_tokens:$mt, response_format:{type:"json_object"},
                messages:[{role:"system",content:$s},{role:"user",content:$u}]}' > "$req"
        else
            jq -n --arg m "$model" --arg s "$(cat "$sysf")" --arg u "$(cat "$usrf")" --argjson mt "$MAX_TOKENS" \
              '{model:$m, max_tokens:$mt,
                messages:[{role:"system",content:$s},{role:"user",content:$u}]}' > "$req"
        fi
    }
    build_request

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
            # An empty or unparseable body is transient far more often than it
            # is fatal: a provider hiccup, a dropped connection, a reply that
            # arrived without content. It used to break out on the first
            # occurrence, spending a whole round on one flaky response.
            *"no parseable content"*|*"unparseable JSON"*)
                log "      $prov/$model returned nothing usable (attempt $attempt/4); waiting ${delay}s"
                sleep "$delay"; delay=$((delay * 2)) ;;
            *response_format*|*json_object*|*"JSON mode"*|*json_schema*)
                if [ "$json_mode" = true ]; then
                    log "      $prov/$model rejected JSON mode; retrying without response_format"
                    json_mode=false; build_request; continue
                fi
                log "      !! $prov/$model call FAILED: $(echo "$err" | head -c 160)"
                break ;;
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

# ---- hollow-approval detection --------------------------------------------
# A THROTTLED reviewer is the nastiest failure here, because a truncated reply
# can still be valid JSON. gpt-oss-120b returned 152 bytes of
# {"verdict":"approve","items":[]} while Cerebras was refusing it on
# tokens-per-minute; mistral-medium-2508 did the same 42 times across week 4.
# Both are indistinguishable from a genuine clean bill of health, and the
# adjudicator converges on them. Size is the tell.
is_hollow() {   # $1 = review json
    local sz v n
    sz=$(wc -c < "$1" 2>/dev/null || echo 0)
    v=$(jq -r '.verdict // ""' "$1" 2>/dev/null)
    n=$(jq -r '(.items // []) | length' "$1" 2>/dev/null)
    [ "$v" = approve ] && [ "${n:-0}" -eq 0 ] && [ "${sz:-0}" -lt "$SUSPICIOUS_REVIEW_BYTES" ]
}

# A review call that comes back hollow is retried once and then declared
# UNAVAILABLE. Week 4's whole failure was that this only logged a warning: the
# verdict still counted as `approve`, so 19 of 23 files were recorded as
# reviewed when no review had taken place. Writing '{}' here makes verdict_of
# report UNAVAILABLE, which the caller must handle -- an absent review can no
# longer masquerade as a passing one.
graded_review() {   # $1 label  $2 prov  $3 model  $4 sysf  $5 usrf  $6 out
    local label=$1 prov=$2 model=$3 sysf=$4 usrf=$5 out=$6 attempt sz
    for attempt in 1 2; do
        json_call "$prov" "$model" "$sysf" "$usrf" "$out"
        if ! is_hollow "$out"; then return 0; fi
        sz=$(wc -c < "$out" 2>/dev/null || echo 0)
        log "      ?? $label ($prov/$model) approved with 0 items in only ${sz} bytes -- suspiciously terse."
        if [ "$attempt" -eq 1 ]; then
            log "      ?? Treating that as no review at all; retrying once in ${SUSPICIOUS_RETRY_PAUSE}s."
            sleep "$SUSPICIOUS_RETRY_PAUSE"
        fi
    done
    log "      !! $label review UNAVAILABLE: two hollow approvals in a row from $prov/$model."
    echo '{}' > "$out"
    return 1
}

# Files whose accuracy review never happened. These are NOT done, whatever the
# working tree looks like, and the run exits nonzero because of them.
NOT_REVIEWED=()
note_not_reviewed() {   # $1 = file  $2 = reason
    local e
    # One entry per file. A file can trip two checks at once (a verification
    # pass that did not run also means its final state was never seen), and the
    # list is a to-do for a human, not a tally of internal states. First reason
    # wins because it is the most specific one.
    for e in ${NOT_REVIEWED[@]+"${NOT_REVIEWED[@]}"}; do
        case "$e" in "$1 -- "*) return 0 ;; esac
    done
    NOT_REVIEWED+=("$1 -- $2")
    log "  !! NOT REVIEWED: $1 ($2)"
}

if [ "$#" -eq 0 ]; then
    echo "Usage: $(basename "$0") <file> [file ...]" >&2; exit 2
fi
TARGETS=("$@")

log "=============================================="
log "fill-doc-todos-free.sh starting"
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
        log "SKIP $REL (no '$MARKER')"; between_files "$index" nosleep; continue
    fi

    n_main=$(grep -cE '/\*\* *'"$MARKER" "$ABS")
    log ""
    log "### $REL  ($n_main declarations to document)"

    if [ "$DRY_RUN" = "true" ]; then
        log "DRY RUN: would fill+review $REL"; between_files "$index" nosleep; continue
    fi

    ORIG="$WORK_DIR/${SAFE}.orig"; CODE_BEFORE="$WORK_DIR/${SAFE}.code"
    cp -f "$ABS" "$ORIG"; code_lines "$ABS" > "$CODE_BEFORE"
    # From here to the digest, an immediate stop must undo this file rather than
    # leave it half-processed. Outside this window ORIG still names the PREVIOUS
    # file, and reverting to it would destroy finished work.
    FILE_IN_PROGRESS=true

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
        if is_cli_provider "$WRITER_PROVIDER"; then cli_writer "$1" "$2"; return $?; fi
        case "$WRITER_PROVIDER" in
            mistral) key_env=MISTRAL_API_KEY ;; cerebras) key_env=CEREBRAS_API_KEY ;;
            openrouter) key_env=OPENROUTER_API_KEY ;; *) key_env=OPENAI_API_KEY ;;
        esac
        python3 "$DIRECT_WRITER" --file "$ABS" --prompt "$1" \
            --model "$WRITER_MODEL" --base-url "$(provider_base "$WRITER_PROVIDER")" \
            --api-key-env "$key_env" --max-tokens "$MAX_TOKENS" \
            --dump "$REVIEWS_DIR/${SAFE}.reply.txt" > "$2" 2>&1
    }

    # The writer on a coding-agent CLI, in two steps: the CLI produces a reply,
    # direct-writer.py applies it.
    #
    # It is NOT given an Edit tool, which is the whole point. The original
    # subscription pipeline drove `claude -p --allowedTools Edit` and let the
    # agent rewrite files itself, and RESULTS.md records two models silently
    # deleting code while filling comments, with every success signal still
    # reporting success. Routing the reply through direct-writer.py keeps the
    # marker-ID worklist, the block protocols, and the per-block refusal of any
    # block containing a non-comment line.
    #
    # Read/Grep/Glob it does get: a writer that can look up the member it is
    # documenting writes better documentation than one guessing from a signature.
    cli_writer() {                       # $1 = prompt file, $2 = log destination
        local bin prompt reply rc=0
        bin=$(cli_binary "$WRITER_PROVIDER")
        prompt="$WORK_DIR/${SAFE}.writer-prompt"
        reply="$REVIEWS_DIR/${SAFE}.reply.txt"

        # ---- agent mode: the writer edits the file itself -----------------
        if [ "$WRITER_MODE" = agent ]; then
            case "$WRITER_PROVIDER" in
                claude-cli)
                    ( cd "$REPO_ROOT" && timeout "$CLI_TIMEOUT" "$bin" \
                        --dangerously-skip-permissions -p --model "$WRITER_MODEL" \
                        --allowedTools "$WRITER_TOOLS" --output-format json ) \
                        < "$1" > "$WORK_DIR/${SAFE}.wraw" 2>>"$2" || rc=$?
                    if [ -s "$WORK_DIR/${SAFE}.wraw" ]; then
                        local cost turns
                        cost=$(jq -r '.total_cost_usd // empty' "$WORK_DIR/${SAFE}.wraw" 2>/dev/null)
                        turns=$(jq -r '.num_turns // empty' "$WORK_DIR/${SAFE}.wraw" 2>/dev/null)
                        jq -r '.result // empty' "$WORK_DIR/${SAFE}.wraw" > "$reply" 2>/dev/null
                        echo "  writer:          agent mode, ${turns:-?} turn(s), \$${cost:-?} against the subscription" >> "$2"
                        if [ "$(jq -r '.is_error // false' "$WORK_DIR/${SAFE}.wraw" 2>/dev/null)" = true ]; then
                            echo "  !! $WRITER_PROVIDER reported an error" >> "$2"; rc=1
                        fi
                    else
                        echo "  !! $WRITER_PROVIDER/$WRITER_MODEL produced no output (rc=$rc)" >> "$2"; rc=1
                    fi ;;
                codex-cli)
                    timeout "$CLI_TIMEOUT" "$bin" exec --model "$WRITER_MODEL" \
                        --dangerously-bypass-approvals-and-sandbox --skip-git-repo-check \
                        -C "$REPO_ROOT" --output-last-message "$reply" - < "$1" >> "$2" 2>&1 || rc=$?
                    ;;
            esac
            # In agent mode nothing parses blocks, so the marker count IS the
            # progress signal. The caller compares it before and after, and
            # integrity_ok runs immediately after this returns.
            echo "  markers left:    $(grep -c "$MARKER" "$ABS" 2>/dev/null || echo '?')" >> "$2"
            return "$rc"
        fi

        # Ask direct-writer.py to assemble the prompt, so the CLI path and the
        # HTTP path cannot drift apart in how the worklist is presented.
        python3 "$DIRECT_WRITER" --file "$ABS" --prompt "$1" \
            --emit-prompt "$prompt" > "$2" 2>&1 || rc=$?
        if [ "$rc" -ne 0 ] || [ ! -s "$prompt" ]; then
            echo "  !! could not assemble the writer prompt" >> "$2"; return 1
        fi

        : > "$reply"
        case "$WRITER_PROVIDER" in
            claude-cli)
                ( cd "$REPO_ROOT" && timeout "$CLI_TIMEOUT" "$bin" \
                    --dangerously-skip-permissions -p --model "$WRITER_MODEL" \
                    --allowedTools Read,Grep,Glob --output-format json ) \
                    < "$prompt" > "$WORK_DIR/${SAFE}.wraw" 2>>"$2" || rc=$?
                if [ "$rc" -eq 0 ] && [ -s "$WORK_DIR/${SAFE}.wraw" ]; then
                    jq -r '.result // empty' "$WORK_DIR/${SAFE}.wraw" > "$reply" 2>/dev/null
                    local cost; cost=$(jq -r '.total_cost_usd // empty' "$WORK_DIR/${SAFE}.wraw" 2>/dev/null)
                    [ -n "$cost" ] && echo "  writer cost:     \$$cost against the subscription" >> "$2"
                fi ;;
            codex-cli)
                timeout "$CLI_TIMEOUT" "$bin" exec --model "$WRITER_MODEL" \
                    --skip-git-repo-check -C "$REPO_ROOT" \
                    --output-last-message "$reply" - < "$prompt" >> "$2" 2>&1 || rc=$?
                ;;
        esac

        if [ ! -s "$reply" ]; then
            echo "  !! $WRITER_PROVIDER/$WRITER_MODEL returned nothing (rc=$rc)" >> "$2"
            return 1
        fi
        python3 "$DIRECT_WRITER" --file "$ABS" --prompt "$1" \
            --reply-file "$reply" >> "$2" 2>&1
    }

    check_pause "before writer: $REL"

    # ---- Writer -------------------------------------------------------------
    # The prompt has to match the mode. -direct tells the model to emit blocks
    # and touch nothing; -agent tells it to edit the file. Handing an agent the
    # block prompt gets you a file that was never edited, which is exactly how
    # the refine silently no-opped for five weeks.
    if [ "$WRITER_MODE" = agent ] && is_cli_provider "$WRITER_PROVIDER"; then
        WRITER_PROMPT_FILE="$PROMPTS_DIR/doc-writer-prompt-agent.txt"
        REFINE_PROMPT_FILE="$PROMPTS_DIR/doc-refine-prompt-agent.txt"
    else
        WRITER_PROMPT_FILE="$PROMPTS_DIR/doc-writer-prompt-direct.txt"
        REFINE_PROMPT_FILE="$PROMPTS_DIR/doc-refine-prompt-direct.txt"
    fi
    render "$ABS" "$WRITER_PROMPT_FILE" > "$WORK_DIR/${SAFE}.wprompt"
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
            sleep_interruptible "$WRITER_PASS_PAUSE" "between writer passes"
        }
        wpass=$((wpass + 1))
    done
    integrity_ok "writer" || { FILE_IN_PROGRESS=false; between_files "$index"; continue; }

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
        FILE_IN_PROGRESS=false
        between_files "$index"; continue
    fi
    log "  writer filled $(( ${markers_before:-0} - ${markers_after:-0} )) marker(s); ${markers_after:-0} left"

    final_acc="$REVIEWS_DIR/${SAFE}.accuracy.json"
    final_sty="$REVIEWS_DIR/${SAFE}.style.json"
    final_adj="$REVIEWS_DIR/${SAFE}.adjudication.json"

    # A review that did not parse is UNAVAILABLE, not "revise". Collapsing
    # the two hid a dead reviewer behind a plausible verdict.
    verdict_of() {
        jq -e 'has("verdict")' "$1" >/dev/null 2>&1 || { echo "UNAVAILABLE"; return; }
        jq -r '.verdict' "$1" 2>/dev/null
    }

    DIFF_BLOCK="$WORK_DIR/${SAFE}.review-input"
    # The adjudicator gets its own, much smaller payload. It is NOT a third
    # reviewer -- its prompt says so -- and it judges only what the two reviewers
    # raised, so the source would be dead weight to it.
    #
    # Dead weight with teeth, as it turned out. When the reviewers started
    # getting the full source, the adjudicator was reusing the same block, and
    # its payload went from about 30 KB to 131 KB. Mistral answered a 131 KB
    # adjudication with an empty body, twice in a row on TrieMap.scala, and each
    # failure cost a whole round. Reviewers need the code; the adjudicator needs
    # the diff and the two opinions.
    ADJ_BLOCK="$WORK_DIR/${SAFE}.adj-input"

    # ---- what the reviewers actually get ------------------------------------
    # The diff alone is not reviewable. Week 4's reviewers were handed `diff -u`
    # (3 lines of context) under a prompt telling them to "read the file", which
    # a chat completion cannot do, and every miss the human later caught needed
    # a method body to see. So: the source first, then the diff, then the
    # repeated-block index.
    build_review_input() {
        local nlines; nlines=$(wc -l < "$ABS")
        {
            echo "=== DIFF OF DOCS TO REVIEW (judge only these additions) ==="
            diff -U "$REVIEW_DIFF_CONTEXT" "$ORIG" "$ABS" || true
            if [ -x "$REPEAT_FINDER" ]; then
                python3 "$REPEAT_FINDER" --orig "$ORIG" --new "$ABS" --min "$REPEAT_GROUP_MIN" 2>/dev/null || true
            fi
        } > "$ADJ_BLOCK"
        {
            if [ "$nlines" -le "$REVIEW_FILE_MAX_LINES" ]; then
                echo "=== FULL SOURCE OF $REL (line-numbered; this is the truth to check against) ==="
                cat -n "$ABS"
            else
                echo "=== SOURCE OF $REL ($nlines lines, too large to inline in full) ==="
                echo "The diff below carries ${REVIEW_DIFF_CONTEXT} lines of context on each side."
                echo "Where that is not enough to see what a member does, say so in the item"
                echo "rather than guessing: set confidence \"low\" and explain what you could not see."
            fi
            echo
            cat "$ADJ_BLOCK"
        } > "$DIFF_BLOCK"
        REVIEW_PAYLOAD_KIND=diff
        [ "$nlines" -le "$REVIEW_FILE_MAX_LINES" ] && REVIEW_PAYLOAD_KIND=source
        if [ "$nlines" -le "$REVIEW_FILE_MAX_LINES" ]; then
            log "    review input: full source ($nlines lines) + diff"
        else
            log "    !! review input: diff -U${REVIEW_DIFF_CONTEXT} ONLY -- $nlines lines exceeds REVIEW_FILE_MAX_LINES=$REVIEW_FILE_MAX_LINES"
            log "    !! this file is being reviewed with less context than the rest; weigh its verdict accordingly"
        fi
    }

    # ---- one accuracy + style pass ------------------------------------------
    # Sets acc_verdict / sty_verdict. Sequential, not parallel: the two calls
    # once raced over a shared temp file, and firing two requests at one
    # provider within a second is exactly what free-tier limits punish.
    # Reviewer independence comes from different models, not from concurrency.
    run_reviewers() {
        build_review_input

        # One shared brief, rendered twice with different emphasis. The two
        # reviewers may be on different transports, and a reviewer must be told
        # the truth about what it can open: one of them can read the repository,
        # the other cannot, and each fails differently if it believes otherwise.
        render_review() {   # $1 = emphasis  $2 = that role's provider  $3 = payload kind
            render "$ABS" "$PROMPTS_DIR/doc-review-prompt.txt" \
              | awk -v e="$1" '{gsub(/\{EMPHASIS\}/, e); print}'
            payload_note "$3"
            tools_note "$2"
            house_rules
            echo; echo "=== SCHEMA (conform exactly) ==="; cat "$SCHEMA"
        }
        local sty_payload_kind=diff
        [ "${STYLE_REVIEW_FULL_SOURCE:-false}" = true ] && sty_payload_kind="$REVIEW_PAYLOAD_KIND"
        # Same emphasis, same brief, both roles. The only text that differs
        # between these two files is the factual description of what each one
        # was handed and what it may open, which has to be true per role.
        render_review "$REVIEW_EMPHASIS" "$ACCURACY_PROVIDER" "$REVIEW_PAYLOAD_KIND" > "$WORK_DIR/${SAFE}.accsys"
        render_review "$REVIEW_EMPHASIS" "$STYLE_PROVIDER"    "$sty_payload_kind"    > "$WORK_DIR/${SAFE}.stysys"

        # The accuracy reviewer may share a provider with the writer; space it.
        # Spacing exists for one API key being hit by several roles in quick
        # succession. Two CLI roles are separate processes against a
        # subscription, so pausing between them buys nothing but wall clock.
        if [ "$ACCURACY_PROVIDER" = "$WRITER_PROVIDER" ] && [ "$PROVIDER_SPACING" -gt 0 ] \
           && ! is_cli_provider "$ACCURACY_PROVIDER"; then
            sleep "$PROVIDER_SPACING"
        fi
        graded_review accuracy "$ACCURACY_PROVIDER" "$ACCURACY_MODEL" \
                      "$WORK_DIR/${SAFE}.accsys" "$DIFF_BLOCK" "$final_acc"
        if [ "$STYLE_PROVIDER" = "$ACCURACY_PROVIDER" ] && [ "$PROVIDER_SPACING" -gt 0 ] \
           && ! is_cli_provider "$STYLE_PROVIDER"; then
            sleep "$PROVIDER_SPACING"
        fi
        # The style reviewer gets the DIFF, not the source. It judges prose
        # conventions, tag rules, voice and altitude, all of which live in the
        # diff; accuracy is the role that has to read method bodies.
        #
        # Sending it the source was killing it on large files. mistral-large
        # copes with about 125 KB and Ordering.scala's review input was 162 KB:
        # four attempts, backing off 30s to 240s, all returning nothing usable,
        # and the file went through its whole review with style UNAVAILABLE.
        # Week 6 would have hit this much harder -- quoted/Quotes.scala is 6208
        # lines before any documentation is added to it.
        #
        # STYLE_REVIEW_FULL_SOURCE=true restores the old behaviour for a run
        # where the style reviewer is on a model with room for it.
        local style_input="$ADJ_BLOCK"
        [ "${STYLE_REVIEW_FULL_SOURCE:-false}" = true ] && style_input="$DIFF_BLOCK"
        local sty_kb; sty_kb=$(( $(wc -c < "$style_input") / 1024 ))
        log "    style payload: ${sty_kb} KB | accuracy payload: $(( $(wc -c < "$DIFF_BLOCK") / 1024 )) KB"
        if ! is_cli_provider "$STYLE_PROVIDER" && [ "$sty_kb" -gt "$MISTRAL_PAYLOAD_LIMIT_KB" ]; then
            # Honest skip beats a doomed retry ladder. The verdict is
            # UNAVAILABLE either way; this way it costs a second, not 7.5
            # minutes, and the log says why.
            log "    !! style review SKIPPED: ${sty_kb} KB exceeds MISTRAL_PAYLOAD_LIMIT_KB=${MISTRAL_PAYLOAD_LIMIT_KB}"
            log "    !! this file is being judged by one reviewer, not two"
            echo '{}' > "$final_sty"
        else
            graded_review style "$STYLE_PROVIDER" "$STYLE_MODEL" \
                          "$WORK_DIR/${SAFE}.stysys" "$style_input" "$final_sty"
        fi

        acc_verdict=$(verdict_of "$final_acc")
        sty_verdict=$(verdict_of "$final_sty")
        log "    reviewers: accuracy=$acc_verdict  style=$sty_verdict"
        # One real accuracy review anywhere in the file's life is the bar. A
        # single dead round is a provider hiccup; never getting one at all is
        # the week-4 failure and must not pass silently.
        [ "$acc_verdict" != UNAVAILABLE ] && acc_ever_ran=true
        return 0
    }

    # ---- adjudication -------------------------------------------------------
    # Sets adj_verdict.
    run_adjudicator() {
        # NOTHING TO ADJUDICATE IS NOT APPROVAL. Handed two empty reviews the
        # adjudicator answers "approve", because it can see no blockers in
        # them -- and that verdict then converges the round or stamps the
        # verification pass as passed. Week 5 hit this on TrieMap.scala: the
        # accuracy reviewer drifted to prose after 26 tool turns, the style
        # reviewer truncated at its output limit, and 49 seconds later the file
        # was recorded as "verification review of that refine: approve" with
        # both reviewers UNAVAILABLE two lines below it in the same digest.
        #
        # This is the week-4 pathology wearing a different hat, so it gets the
        # same answer: an absent review is UNAVAILABLE, never a pass.
        if [ "$acc_verdict" = UNAVAILABLE ] && [ "$sty_verdict" = UNAVAILABLE ]; then
            log "    !! both reviews unavailable -- NOT adjudicating; this pass reviewed nothing"
            echo '{}' > "$final_adj"
            adj_verdict=UNAVAILABLE
            return 0
        fi
        check_pause "before adjudication: $REL"
        # Space this away from the writer call on the same provider. The reviews
        # above take a few seconds; on a free tier that is not enough headroom.
        if [ "$ADJUDICATOR_PROVIDER" = "$WRITER_PROVIDER" ] && [ "$PROVIDER_SPACING" -gt 0 ] \
           && ! is_cli_provider "$ADJUDICATOR_PROVIDER"; then
            sleep "$PROVIDER_SPACING"
        fi
        log "    adjudicating ($ADJUDICATOR_PROVIDER/$ADJUDICATOR_MODEL)..."
        { render "$ABS" "$PROMPTS_DIR/doc-adjudicator-prompt.txt"; house_rules
          echo; echo "=== ADJUDICATION SCHEMA (conform exactly) ==="; cat "$ADJ_SCHEMA"; } > "$WORK_DIR/${SAFE}.adjsys"
        # The two reviews are the substance of an adjudication; the diff is
        # context for overruling a reviewer who is wrong about the code. On a
        # file whose diff is enormous, the context is what has to give, because
        # the alternative is no adjudication at all and therefore no refine.
        local adj_kb; adj_kb=$(( $(wc -c < "$ADJ_BLOCK") / 1024 ))
        { if is_cli_provider "$ADJUDICATOR_PROVIDER" || [ "$adj_kb" -le "$MISTRAL_PAYLOAD_LIMIT_KB" ]; then
              cat "$ADJ_BLOCK"
          else
              log "    !! diff omitted from adjudication: ${adj_kb} KB exceeds MISTRAL_PAYLOAD_LIMIT_KB=${MISTRAL_PAYLOAD_LIMIT_KB}"
              echo "=== DIFF OMITTED ==="
              echo "The diff for this file is ${adj_kb} KB, too large to include."
              echo "Judge from the two reviews below. Merge and rank what they raised."
              echo "You cannot verify a reviewer's claim against the code here, so do"
              echo "NOT overrule one for being wrong about the code: you cannot see it."
              echo "Where a claim looks doubtful, keep it and lower its severity."
          fi
          echo; echo "=== ACCURACY REVIEW (JSON) ==="; cat "$final_acc"
          echo; echo "=== STYLE REVIEW (JSON) ==="; cat "$final_sty"; } > "$WORK_DIR/${SAFE}.adjusr"
        log "    adjudication payload: $(( $(wc -c < "$WORK_DIR/${SAFE}.adjusr") / 1024 )) KB"
        json_call "$ADJUDICATOR_PROVIDER" "$ADJUDICATOR_MODEL" \
                  "$WORK_DIR/${SAFE}.adjsys" "$WORK_DIR/${SAFE}.adjusr" "$final_adj"

        # A DEAD ADJUDICATOR IS NOT A VERDICT. `.verdict // "revise"` turned a
        # failed call into a confident-looking "revise" carrying an empty
        # worklist, and the writer was then sent to "work the adjudicated list"
        # against nothing at all. Seen on TrieMap.scala in the week-5 rerun:
        # devstral returned no content, and the round became a no-op refine that
        # burned one of the three.
        #
        # Same rule as everywhere else in this script: an absent answer is
        # UNAVAILABLE, never a decision. verdict_of reports that, and the caller
        # skips the refine and tries the next round.
        adj_verdict=$(verdict_of "$final_adj")
        local n_items n_dis
        n_items=$(jq -r '(.resolved_items // []) | length' "$final_adj" 2>/dev/null || echo 0)
        n_dis=$(jq -r '(.disagreements // []) | length' "$final_adj" 2>/dev/null || echo 0)
        # Show the severity split, not just the count. Severity is what decides
        # convergence, refine churn and the NOT-REVIEWED list, and inflation is
        # invisible in a bare total: week 6's FunctionExtensions reported "34
        # item(s)" for four rounds while every one of them was graded a blocker
        # and nearly all were requests to reword a method like its siblings.
        local n_block n_nit
        n_block=$(jq -r '[(.resolved_items // [])[] | select(.severity=="blocker")] | length' "$final_adj" 2>/dev/null || echo 0)
        n_nit=$(jq -r '[(.resolved_items // [])[] | select(.severity=="nit")] | length' "$final_adj" 2>/dev/null || echo 0)
        log "    adjudicator: $adj_verdict  (${n_items} item(s): ${n_block} blocker, ${n_nit} nit; ${n_dis} disagreement(s) settled)"

        # NITS ALONE NEVER BLOCK -- the adjudicator's own rule, enforced here
        # because it does not always follow it. On TrieMap.scala round 3 both
        # reviewers APPROVED, and the adjudicator answered "revise" with 63
        # items, 25 of them nits. 61 edits then landed on prose that had just
        # been passed, and the next accuracy review went from approve to revise
        # with three blockers about claims the rewritten text now made.
        #
        # Churning approved prose over wording preferences is not free: every
        # edit is a chance to introduce a claim the code does not support. If
        # nothing in the worklist is a blocker, the file is done.
        local adj_blockers
        adj_blockers=$(jq -r '[(.resolved_items // [])[] | select(.severity=="blocker")] | length' "$final_adj" 2>/dev/null || echo 0)
        if [ "$adj_verdict" = "revise" ] && [ "${adj_blockers:-0}" -eq 0 ] && [ "${n_items:-0}" -gt 0 ]; then
            log "    ...but 0 of those ${n_items} items are blockers; nits alone never block, so this is an approve"
            adj_verdict=approve
        fi
    }

    # verified_final answers the only question that matters at the end: was the
    # file, AS IT NOW STANDS ON DISK, looked at by a working accuracy reviewer?
    # Not "did one ever run" (acc_ever_ran, which stays true even if the last
    # three edits went unseen) and not "did the adjudicator approve" (which it
    # will do on an empty pass). It is set only where the two coincide: a round
    # that converged with a usable accuracy review and no edit after it, or a
    # verification pass with a usable accuracy review.
    round=1; converged=false; final_refinement=false
    acc_ever_ran=false; verified_final=false; verify_verdict=skipped; refine_failed=0
    while [ "$round" -le "$MAX_ROUNDS" ]; do
        check_pause "before review round $round: $REL"
        log "  round $round/$MAX_ROUNDS: accuracy ($ACCURACY_MODEL) then style ($STYLE_MODEL)..."

        run_reviewers
        run_adjudicator

        if [ "$adj_verdict" = "approve" ]; then
            converged=true
            [ "$acc_verdict" != UNAVAILABLE ] && verified_final=true
            break
        fi

        # A round that reviewed nothing has no worklist to refine against.
        # Refining from an empty adjudication would send the writer off to
        # rewrite prose on no evidence, which is worse than leaving it alone.
        if [ "$adj_verdict" = UNAVAILABLE ]; then
            log "    round $round reviewed nothing; skipping the refine and trying again"
            round=$((round + 1))
            continue
        fi

        if [ "$round" -eq "$MAX_ROUNDS" ]; then
            final_refinement=true
            log "    final refine (a verification review follows; see FINAL_VERIFY)..."
        else
            log "    refine: working the adjudicated list..."
        fi

        check_pause "before refine round $round: $REL"
        refine_hash_before=$(md5sum "$ABS" | cut -d" " -f1)
        # doc-refine-prompt-DIRECT, not -agent. The -agent prompt was written for
        # a coding-agent client that edits files itself (the old Claude Code and
        # aider pipeline) and it names no reply format at all. When the writer
        # moved to direct-writer.py, the FILL path got a direct prompt spelling
        # out the ID-block protocol and the refine path was left on the agent
        # one -- so every refine reply came back as prose plus a ```scala
        # snippet, parsed to zero blocks, and was discarded.
        #
        # That was true of EVERY refine in every run, weeks 1 through 5: 100% of
        # them logged `blocks parsed: 0  applied: 0` and nothing ever noticed.
        # The whole review loop was ornamental. Every doc comment shipped so far
        # is the writer's first pass, which is why week 4 drew 51 review comments
        # after "two rounds of review", and why reviewers kept re-finding the
        # same items on text that never changed.
        { render "$ABS" "$REFINE_PROMPT_FILE"; house_rules
          echo; cat "$final_adj"; } > "$WORK_DIR/${SAFE}.rprompt"
        run_writer "$WORK_DIR/${SAFE}.rprompt" "$REVIEWS_DIR/${SAFE}.refine${round}.log"
        integrity_ok "refine round $round" || break

        # A refine that applied nothing is a failed refine, and it must say so.
        # The counters were printed all along; nothing read them, so a no-op
        # round looked exactly like a successful one.
        # In blocks mode the applier reports how many landed. In agent mode
        # nothing counts edits, so the question becomes "did the file change at
        # all", answered by hashing it either side of the call. Either way a
        # refine that changed nothing against a non-empty worklist is a failure
        # and has to say so.
        if [ "$WRITER_MODE" = agent ] && is_cli_provider "$WRITER_PROVIDER"; then
            [ "$refine_hash_before" != "$(md5sum "$ABS" | cut -d" " -f1)" ] && r_applied=1 || r_applied=0
        else
            r_applied=$(grep -oE "^ *applied: +[0-9]+" "$REVIEWS_DIR/${SAFE}.refine${round}.log" 2>/dev/null | awk '{print $2}' | head -1)
        fi
        r_items=$(jq -r '(.resolved_items // []) | length' "$final_adj" 2>/dev/null || echo 0)
        if [ "${r_applied:-0}" -eq 0 ] && [ "${r_items:-0}" -gt 0 ]; then
            log "    !! refine applied NOTHING against ${r_items} adjudicated item(s)"
            log "    !! the reviewers' findings did not reach the file; see ${SAFE}.refine${round}.log"
            refine_failed=$(( refine_failed + 1 ))
        else
            if [ "$WRITER_MODE" = agent ] && is_cli_provider "$WRITER_PROVIDER"; then
                log "    refine edited the file against ${r_items} item(s)"
            else
                log "    refine applied ${r_applied:-0} edit(s) against ${r_items} item(s)"
            fi
        fi

        [ "$final_refinement" = "true" ] && break
        round=$((round + 1))
    done

    # ---- verification review ------------------------------------------------
    # The refine after the last round used to ship unlooked-at: 23 of 26 files
    # in week 4 ended on "final refine (not re-reviewed)", so the LAST edit made
    # to almost every file was the one nobody checked. This pass does not feed
    # another refine -- there is no budget for an endless loop -- but it records
    # a real verdict, and a `revise` here means the file needs human eyes.
    if [ "$final_refinement" = "true" ] && [ "$FINAL_VERIFY" = "true" ]; then
        check_pause "before verification review: $REL"
        log "  verification review of the final refine (no further refine follows)..."
        run_reviewers
        run_adjudicator
        verify_verdict="$adj_verdict"
        if [ "$acc_verdict" = UNAVAILABLE ]; then
            verify_verdict="UNAVAILABLE"
            note_not_reviewed "$REL" "verification review did not run (accuracy=$acc_verdict style=$sty_verdict)"
        elif [ "$adj_verdict" = UNAVAILABLE ]; then
            # The file WAS looked at; only the summing-up went missing. Worth a
            # human's eye, but for a different reason than open blockers, and
            # the list is read by a person who deserves the accurate one.
            verified_final=true
            note_not_reviewed "$REL" "verification reviewed the file but the adjudicator never answered"
        elif [ "$adj_verdict" != "approve" ]; then
            verified_final=true   # it WAS reviewed; it just did not come back clean
            note_not_reviewed "$REL" "final refine still has open blockers after $MAX_ROUNDS rounds"
        else
            verified_final=true
        fi
    fi

    # An accuracy review that never ran is the week-4 failure. Say so loudly and
    # keep the file out of the "done" pile, whatever is in the working tree.
    if [ "$acc_ever_ran" != "true" ]; then
        note_not_reviewed "$REL" "accuracy reviewer was UNAVAILABLE ($ACCURACY_PROVIDER/$ACCURACY_MODEL)"
    elif [ "$refine_failed" -gt 0 ] && [ "$converged" != "true" ]; then
        note_not_reviewed "$REL" "$refine_failed refine(s) applied nothing; reviewer findings never reached the file"
    elif [ "$verified_final" != "true" ]; then
        # Reviews happened, but not on what is now on disk: the last usable one
        # was followed by an edit, or the rounds ran out mid-loop.
        note_not_reviewed "$REL" "the file's final state was never seen by a working accuracy reviewer"
    fi

    # ---- Digest -------------------------------------------------------------
    DIGEST="$REVIEWS_DIR/${SAFE}.digest.md"
    {
        echo "# Doc review digest: $REL"; echo
        echo "- models: writer $WRITER_MODEL | accuracy $ACCURACY_MODEL | style $STYLE_MODEL | adjudicator $ADJUDICATOR_MODEL"
        echo "- converged: $converged (up to $MAX_ROUNDS rounds)"
        echo "- final refinement after review limit: $final_refinement"
        echo "- verification review of that refine: $verify_verdict"
        echo "- a real accuracy review ran at some point: $acc_ever_ran"
        echo "- the file AS IT NOW STANDS was accuracy-reviewed: $verified_final"
        echo "- accuracy verdict: $(jq -r '.verdict // "UNAVAILABLE"' "$final_acc" 2>/dev/null)"
        echo "- style verdict: $(jq -r '.verdict // "UNAVAILABLE"' "$final_sty" 2>/dev/null)"
        echo "- ADJUDICATOR verdict (final): $(jq -r '.verdict // "?"' "$final_adj" 2>/dev/null)"
        if [ "$verified_final" != "true" ] || { [ "$verify_verdict" != "approve" ] && [ "$verify_verdict" != "skipped" ]; }; then
            echo
            echo "> **NOT REVIEWED.** Do not put this file in a PR on the strength of this"
            echo "> digest. Read the diff yourself, or re-run the file once the reviewer is"
            echo "> healthy. See reviews/NOT-REVIEWED.txt."
        fi
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
    FILE_IN_PROGRESS=false
    between_files "$index"
done

log ""
log "=============================================="
log "COMPLETE. Reviews + digests in: $REVIEWS_DIR"
log "Changes are in the working tree, UNCOMMITTED."

# The run's most important output. Week 4 had no equivalent: 19 files went into
# a PR carrying an approval nobody had actually given, and the first person to
# find out was the human reviewer, 51 comments later.
if [ "${#NOT_REVIEWED[@]}" -gt 0 ]; then
    : > "$NOT_REVIEWED_FILE"
    log ""
    log "!! ${#NOT_REVIEWED[@]} file(s) are FILLED BUT NOT REVIEWED:"
    for entry in "${NOT_REVIEWED[@]}"; do
        log "!!   $entry"
        echo "$entry" >> "$NOT_REVIEWED_FILE"
    done
    log "!! Listed in: $NOT_REVIEWED_FILE"
    log "!! Read these diffs yourself, or re-run those files, before opening the PR."
    log "=============================================="
    exit 3
fi

rm -f "$NOT_REVIEWED_FILE"
log "Every file got a real accuracy review. Next: run adversarial-gate.sh over"
log "the partition, then fold the human's PR feedback into docs/house-rules.md."
log "=============================================="
