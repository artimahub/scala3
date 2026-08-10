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

cd "$REPO"
export PATH="$PATH:/home/node/.local/bin"
export BROWSER=/bin/true          # aider offers to open docs URLs; --yes-always accepts
set -a; . /home/node/.aider/.env; set +a

if pgrep -f "bin/aider --model" >/dev/null || pgrep -f "pool exec" >/dev/null; then
    echo "REFUSING: another trial is already running (they share $TARGET)." >&2
    exit 2
fi
[ -f "$INPUT" ] || { echo "Missing marked input: $INPUT" >&2; exit 2; }

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
stopped_because="max rounds"

run_one_round() {
    case "$PROVIDER" in
      cerebras)
        export OPENAI_API_BASE="$CEREBRAS_API_BASE"
        export OPENAI_API_KEY="$CEREBRAS_API_KEY"
        timeout --signal=TERM "$TIMEOUT" \
          aider --model "$MODEL" --edit-format "$EDIT_FORMAT" \
            --message-file "$EXP/prompt.aider.txt" \
            --yes-always --no-auto-commits --no-gitignore \
            --map-tokens 0 --no-stream --no-check-update --no-analytics \
            --no-show-model-warnings \
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
    echo "    round $round: $before -> $after markers  ($((SECONDS - rstart))s, client exit $rc)"
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
  echo "elapsed:           ${elapsed}s   (last client exit $rc)"
  echo "stopped because:   $stopped_because"
  echo "marker progression:$progression   (started at 128)"
  echo "markers left:      $markers"
  echo "CODE INTEGRITY:    $integrity"
  echo "Failure+Success:   $classes/2 classes present"
  echo "redundant @return: $redundant"
  echo "tag lines ending '.': $tag_dot of $tag_tot  (house style: omit)"
} | tee "$EXP/$LABEL.result.txt"
