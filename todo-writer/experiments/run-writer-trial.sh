#!/usr/bin/env bash
#
# run-writer-trial.sh <label> <provider> <model> [edit-format]
#
#   provider = cerebras   -> aider, OpenAI-compatible, SEARCH/REPLACE prompt
#   provider = poolside   -> pool exec (agent), targeted-patch prompt
#
# Runs ONE writer model over the same marked input and scores it against the
# same guards, so trials are comparable across providers.
#
# Two clients are unavoidable: pool sends Anthropic-style cache_control fields
# that Cerebras rejects, and aider's edit formats defeated poolside's laguna (it
# cannot emit SEARCH/REPLACE, and whole-file mode deleted a public class). Each
# provider gets the client and prompt it works with; the INPUT and the GUARDS
# are identical, which is what makes the comparison fair.
#
# ---------------------------------------------------------------------------
# WHY THIS LOOPS
#
# The writer prompt tells the model "you are NOT expected to finish the file in
# one reply; a partial reply of correct, complete blocks is a success". That
# exists because laguna would otherwise spend its whole output budget narrating
# and fill nothing. The cost is that a capable model takes it literally:
# gpt-oss-120b reasoned "given token limit, maybe we can fill a subset", filled
# 65 of 128 markers and stopped, clean exit 0.
#
# The markers ARE the ledger. Whatever a round does not reach is still marked,
# so the next round picks it up. This loop is the other half of that prompt, and
# without it the instruction just caps the result.
#
# It also paces. On the Cerebras free tier the run did not die from the partial
# pass but from what came after: nine SEARCH blocks failed to match, aider asked
# the model to re-emit them, and every retry hit the tokens-per-minute cap until
# aider gave up. ROUND_PAUSE lets that window recover between rounds.
# ---------------------------------------------------------------------------
#
# Guards, in the order they matter:
#   1. CODE INTEGRITY - strip comment lines and diff. A trial once deleted an
#      entire public class while the marker count, the edit count and the
#      diffstat all reported success; only this caught it. Checked EVERY round,
#      and a round that trips it is rolled back and the trial stops.
#   2. Markers remaining - 0 means every placeholder was filled.
#   3. Redundant @return - the project's DROP rule, the most-missed convention.
#   4. Trailing periods on tag lines - a house-style deviation seen on Cerebras.
#
# Note that a client's exit code is NOT the completion signal: aider exited 0
# having abandoned its retries with 63 markers left. The marker count decides.
#
# --no-auto-lint is REQUIRED, not tidiness. Aider lints edited files with a
# tree-sitter grammar that does not understand Scala 3 capture checking, so it
# reports untouched original source as broken:
#
#     def flatMap[U](f: T => Try[U]^): Try[U]^{this, f}     <- flagged
#
# and then asks the model to "Fix any errors below". That spends rounds on
# nothing and, worse, actively invites the model to edit Scala code, which is
# the one thing the writer prompt forbids and what the integrity guard exists to
# catch. Four such prompts appeared in one gpt-oss-120b run before this was
# spotted. Poolside is unaffected (pool has no linter), so leaving it on would
# also have made the two providers incomparable.
#
# Output goes under experiments/, on the bind mount, so it survives a rebuild.
#
# Env: MAX_ROUNDS=6  ROUND_PAUSE=60  TIMEOUT=3600
#
# NOTE: trials share one target file. Do not run two at once.

set -uo pipefail

LABEL=${1:?usage: run-writer-trial.sh <label> <provider: cerebras|poolside> <model> [edit-format]}
PROVIDER=${2:?usage: run-writer-trial.sh <label> <provider: cerebras|poolside> <model> [edit-format]}
MODEL=${3:?usage: run-writer-trial.sh <label> <provider: cerebras|poolside> <model> [edit-format]}
EDIT_FORMAT=${4:-diff}
TIMEOUT=${TIMEOUT:-3600}
MAX_ROUNDS=${MAX_ROUNDS:-6}
ROUND_PAUSE=${ROUND_PAUSE:-60}

REPO=/workspace/scala3
EXP="$REPO/todo-writer/experiments/try-scala"
TARGET=library/src/scala/util/Try.scala
INPUT="$EXP/Try.scala.MARKED-input"

# Model metadata. Lives on the persisted volume rather than in the repo because
# it sits beside the API keys; it survives a devcontainer rebuild.
#
# This is REQUIRED, not tuning. Without an entry aider falls back to a
# conservative output cap, and zai-glm-4.7 is a reasoning model on Cerebras: it
# emits into a separate `reasoning` field before any `content`, so the fallback
# cap was consumed before it produced a single character. Aider reported
# "Output tokens: ~0" and the trial scored 128 -> 128, "no progress in round 1",
# which reads as a model failure and was not one. Given a real budget the same
# model emitted 42 of 42 SEARCH/REPLACE blocks, all matching byte-exactly, in
# one 5-second reply.
#
# The reasoning overhead is not proportional: 216 tokens to answer "PONG", but
# only 934 of 7,147 on the real task. Judge it on real work, not a toy prompt.
METADATA=${METADATA:-/home/node/.aider/model-metadata.json}

cd "$REPO"
export PATH="$PATH:/home/node/.local/bin"
export BROWSER=/bin/true          # aider offers to open docs URLs; --yes-always accepts
set -a; . /home/node/.aider/.env; set +a

if pgrep -f "bin/aider --model" >/dev/null || pgrep -f "pool exec" >/dev/null; then
    echo "REFUSING: another trial is already running (they share $TARGET)." >&2
    exit 2
fi
[ -f "$INPUT" ] || { echo "Missing marked input: $INPUT" >&2; exit 2; }
# Fail loudly rather than silently running with aider's fallback caps, which is
# what made zai-glm-4.7 look broken.
if [ "$PROVIDER" = cerebras ] && ! grep -q "\"${MODEL}\"" "$METADATA" 2>/dev/null; then
    echo "No metadata entry for '$MODEL' in $METADATA." >&2
    echo "Add one (max_output_tokens 32000) or aider will use a conservative default cap." >&2
    exit 2
fi

code_lines() { grep -vE '^\s*(\*|/\*\*|\*/)' "$1" | grep -v '^\s*$'; }
# grep -c prints 0 and exits 1 when there are no matches; `|| true` keeps the
# count without the stray second value an `|| echo 0` would append.
count_markers() { grep -c "TODO FILL IN" "$TARGET" 2>/dev/null || true; }

echo "=== trial: $LABEL  (provider $PROVIDER, model $MODEL) ==="
cp -f "$INPUT" "$TARGET"
code_lines "$TARGET" > "$EXP/.code.before"
start_markers=$(count_markers)
echo "start: $start_markers markers"

start=$SECONDS
rc=0
integrity=PASS
progression="$start_markers"
# Timing. `wall` is everything; `paused` is only this script's own sleeps
# between rounds. Subtracting them gives the time actually spent waiting on the
# model and the service, which is the number to compare providers on -- a model
# that needs four rounds would otherwise be charged for three extra pauses and
# look slower than it is. Rate-limit backoff inside a round is deliberately NOT
# subtracted: that is the service being slow, which is a real cost of using it.
paused=0
round_times=""
rounds_run=0
stopped_because="max rounds"

# Ctrl-C handling. aider traps SIGINT itself (it uses it to interrupt a
# response, not to exit), so an interactive Ctrl-C leaves the client alive AND
# lets this loop start another round. The client therefore runs in the
# background so we hold its PID and can kill it, and the loop exits rather than
# continuing. Without this the only way to stop a trial is to find and kill
# three processes by hand, script first.
#
# Killing the client PID alone is not enough: it is a subshell whose children
# (timeout, then aider or pool) survive it. `set -m` gives each background job
# its own process group so a negative-PID kill reaches the whole tree, and the
# pkill sweep afterwards catches anything that escaped into another group.
set -m
client_pid=""
kill_client_tree() {
    [ -n "$client_pid" ] || return 0
    kill -TERM -"$client_pid" 2>/dev/null || kill -TERM "$client_pid" 2>/dev/null
    sleep 2
    kill -9 -"$client_pid" 2>/dev/null || kill -9 "$client_pid" 2>/dev/null
    # Anything that reparented or changed group.
    pkill -9 -P "$client_pid" 2>/dev/null
}
on_interrupt() {
    echo ""
    echo "!! interrupted -- stopping the client and ending the trial"
    kill_client_tree
    echo "   $TARGET is left mid-edit; the next trial resets it from the baseline."
    echo "   If a client somehow survives, it will be a '$PROVIDER' process; kill it by PID."
    exit 130
}
trap on_interrupt INT TERM

run_one_round() {
    (
    case "$PROVIDER" in
      cerebras)
        export OPENAI_API_BASE="$CEREBRAS_API_BASE"
        export OPENAI_API_KEY="$CEREBRAS_API_KEY"
        timeout --signal=TERM "$TIMEOUT" \
          aider --model "$MODEL" --edit-format "$EDIT_FORMAT" \
            --message-file "$EXP/prompt.aider.txt" \
            --yes-always --no-auto-commits --no-gitignore \
            --map-tokens 0 --no-stream --no-check-update --no-analytics \
            --no-show-model-warnings --no-auto-lint \
            --model-metadata-file "$METADATA" \
            --chat-history-file "$EXP/$LABEL.chat.md" \
            --input-history-file "$EXP/$LABEL.input" \
            --llm-history-file "$EXP/$LABEL.llm" \
            "$TARGET" >> "$EXP/$LABEL.log" 2>&1
        ;;
      poolside)
        export POOLSIDE_API_KEY="$OPENAI_API_KEY"
        export POOLSIDE_STANDALONE_BASE_URL="https://inference.poolside.ai"
        export POOLSIDE_STANDALONE_MODEL="$MODEL"
        timeout --signal=TERM "$TIMEOUT" \
          pool exec -f "$EXP/prompt.agent.txt" -d "$REPO" --unsafe-auto-allow \
            >> "$EXP/$LABEL.log" 2>&1
        ;;
      *)
        echo "Unknown provider: $PROVIDER (want cerebras or poolside)" >&2; exit 2 ;;
    esac
    ) &
    client_pid=$!
    wait "$client_pid"
    local st=$?
    client_pid=""
    return $st
}

: > "$EXP/$LABEL.log"
for round in $(seq 1 "$MAX_ROUNDS"); do
    before=$(count_markers)
    if [ "$before" -eq 0 ]; then stopped_because="all markers filled"; break; fi

    # Snapshot so a round that corrupts code can be rolled back to the state
    # before it, keeping the good work from earlier rounds.
    cp -f "$TARGET" "$EXP/.round.snapshot"

    echo "--- round $round/$MAX_ROUNDS: $before markers remaining ---"
    rstart=$SECONDS
    run_one_round
    rc=$?
    after=$(count_markers)
    rsecs=$((SECONDS - rstart))
    rounds_run=$round
    round_times="${round_times}${round_times:+, }r${round}=${rsecs}s"
    echo "    round $round: $before -> $after markers  (${rsecs}s, client exit $rc)"
    progression="$progression -> $after"

    if ! code_lines "$TARGET" | diff -q "$EXP/.code.before" - >/dev/null; then
        echo "    !! CODE INTEGRITY FAILURE in round $round; rolling this round back and stopping"
        code_lines "$TARGET" | diff "$EXP/.code.before" - | head -20
        cp -f "$EXP/.round.snapshot" "$TARGET"
        integrity=FAIL
        stopped_because="code integrity failure in round $round"
        break
    fi

    if [ "$after" -eq 0 ]; then stopped_because="all markers filled"; break; fi
    if [ "$after" -eq "$before" ]; then stopped_because="no progress in round $round"; break; fi

    [ "$round" -lt "$MAX_ROUNDS" ] && [ "$ROUND_PAUSE" -gt 0 ] && {
        echo "    pausing ${ROUND_PAUSE}s to let the rate-limit window recover"
        sleep "$ROUND_PAUSE"
        paused=$((paused + ROUND_PAUSE))
    }
done

elapsed=$((SECONDS - start))
cp -f "$TARGET" "$EXP/Try.scala.$LABEL"
rm -f "$EXP/.round.snapshot"

markers=$(count_markers)
classes=$(grep -cE '^final case class (Failure|Success)' "$TARGET" 2>/dev/null || true)
tag_tot=$(grep -cE '^\s*\*\s*@(param|tparam|return) ' "$TARGET" 2>/dev/null || true)
tag_dot=$(grep -cE '^\s*\*\s*@(param|tparam|return) .*\.$' "$TARGET" 2>/dev/null || true)

redundant=$(python3 - "$TARGET" <<'PY'
import re,sys
L=open(sys.argv[1]).read().splitlines(); v=0; i=0
while i<len(L):
    m=re.match(r'\s*/\*\* (Returns .*)',L[i])
    if m and '*/' not in L[i]:
        d=m.group(1).rstrip(); j=i+1
        while j<len(L) and '*/' not in L[j]:
            r=re.match(r'\s*\*\s*@return (.*)',L[j])
            if r:
                t=r.group(1).strip()
                if 'exception' not in t.lower() and len(t)<=len(d): v+=1
            j+=1
        i=j
    i+=1
print(v)
PY
)

{
  echo "label:             $LABEL"
  echo "provider/model:    $PROVIDER / $MODEL"
  echo "rounds run:        $rounds_run   ($round_times)"
  echo "WORK TIME:         $((elapsed - paused))s   <- compare providers on this"
  echo "wall clock:        ${elapsed}s   (includes ${paused}s of this script's own between-round pauses)"
  echo "per-marker:        $(awk -v t="$((elapsed - paused))" -v f="$((start_markers - markers))" \
                             'BEGIN{ if (f>0) printf "%.1fs per marker filled", t/f; else print "n/a (nothing filled)" }')"
  echo "last client exit:  $rc"
  echo "stopped because:   $stopped_because"
  echo "marker progression:$progression   (started at 128)"
  echo "markers left:      $markers"
  echo "CODE INTEGRITY:    $integrity"
  echo "Failure+Success:   $classes/2 classes present"
  echo "redundant @return: $redundant"
  echo "tag lines ending '.': $tag_dot of $tag_tot  (house style: omit)"
} | tee "$EXP/$LABEL.result.txt"
