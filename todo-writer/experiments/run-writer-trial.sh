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
# Two clients are needed because no single one works for both: pool emits
# Anthropic-style cache_control fields that Cerebras rejects, and aider's edit
# formats defeated poolside's laguna (it cannot emit SEARCH/REPLACE, and
# whole-file mode deleted a public class). Each provider gets the client and
# prompt it actually works with; the INPUT and the GUARDS are identical, which
# is what makes the comparison fair.
#
# Guards, in the order they matter:
#   1. CODE INTEGRITY - strip comment lines before and after and diff them.
#      A trial once deleted an entire public class while the marker count, the
#      edit count and the diffstat all reported success; only this caught it.
#   2. Markers remaining - 0 means every placeholder was filled.
#   3. Redundant @return - the project's DROP rule, the most-missed convention.
#   4. Trailing periods on tag lines - a house-style deviation seen on Cerebras.
#
# Output goes under experiments/, on the bind mount, so it survives a rebuild.
#
# NOTE: trials share one target file. Do not run two at once.

set -uo pipefail

LABEL=${1:?usage: run-writer-trial.sh <label> <provider: cerebras|poolside> <model> [edit-format]}
PROVIDER=${2:?usage: run-writer-trial.sh <label> <provider: cerebras|poolside> <model> [edit-format]}
MODEL=${3:?usage: run-writer-trial.sh <label> <provider: cerebras|poolside> <model> [edit-format]}
EDIT_FORMAT=${4:-diff}
TIMEOUT=${TIMEOUT:-3600}

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

echo "=== trial: $LABEL  (provider $PROVIDER, model $MODEL) ==="
cp -f "$INPUT" "$TARGET"
code_lines() { grep -vE '^\s*(\*|/\*\*|\*/)' "$1" | grep -v '^\s*$'; }
code_lines "$TARGET" > "$EXP/.code.before"
start=$SECONDS

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
        "$TARGET" > "$EXP/$LABEL.log" 2>&1
    rc=$?
    ;;
  poolside)
    export POOLSIDE_API_KEY="$OPENAI_API_KEY"
    export POOLSIDE_STANDALONE_BASE_URL="https://inference.poolside.ai"
    export POOLSIDE_STANDALONE_MODEL="$MODEL"
    timeout --signal=TERM "$TIMEOUT" \
      pool exec -f "$EXP/prompt.agent.txt" -d "$REPO" --unsafe-auto-allow \
        > "$EXP/$LABEL.log" 2>&1
    rc=$?
    ;;
  *)
    echo "Unknown provider: $PROVIDER (want cerebras or poolside)" >&2; exit 2 ;;
esac

elapsed=$((SECONDS - start))
cp -f "$TARGET" "$EXP/Try.scala.$LABEL"

markers=$(grep -c "TODO FILL IN" "$TARGET" 2>/dev/null || true)
code_lines "$TARGET" > "$EXP/.code.after"
if diff -q "$EXP/.code.before" "$EXP/.code.after" >/dev/null; then integrity=PASS; else integrity=FAIL; fi
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
  echo "exit:              $rc      elapsed: ${elapsed}s"
  echo "markers left:      $markers   (0 = fully filled; input had 128)"
  echo "CODE INTEGRITY:    $integrity"
  echo "Failure+Success:   $classes/2 classes present"
  echo "redundant @return: $redundant"
  echo "tag lines ending '.': $tag_dot of $tag_tot  (house style: omit)"
} | tee "$EXP/$LABEL.result.txt"
