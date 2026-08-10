#!/usr/bin/env bash
#
# run-writer-trial.sh <label> <aider-model> [edit-format]
#
# Runs ONE writer model over the same marked input file and scores it against
# the same guards, so trials are comparable. Resets the target file first, so
# each run starts from byte-identical input.
#
# Guards, in the order they matter:
#   1. CODE INTEGRITY - strip comment lines before and after and diff them.
#      A client trial once deleted an entire public class while the marker
#      count, the edit count and the diffstat all reported success; only this
#      check caught it. A run that fails this is a failure however good the prose.
#   2. Markers remaining - 0 means every placeholder was filled.
#   3. Redundant @return - the project's DROP rule, the most-missed convention.
#
# Output is written under experiments/, which is on the bind mount and so
# survives a devcontainer rebuild (unlike /tmp).

set -uo pipefail

LABEL=${1:?usage: run-writer-trial.sh <label> <aider-model> [edit-format]}
MODEL=${2:?usage: run-writer-trial.sh <label> <aider-model> [edit-format]}
EDIT_FORMAT=${3:-diff}
TIMEOUT=${TIMEOUT:-2400}

REPO=/workspace/scala3
EXP="$REPO/todo-writer/experiments/try-scala"
TARGET=library/src/scala/util/Try.scala
INPUT="$EXP/Try.scala.MARKED-input"

cd "$REPO"
export BROWSER=/bin/true          # aider offers to open docs URLs; --yes-always accepts
set -a; . /home/node/.aider/.env; set +a
export OPENAI_API_BASE="$CEREBRAS_API_BASE"
export OPENAI_API_KEY="$CEREBRAS_API_KEY"

echo "=== trial: $LABEL  (model $MODEL, edit-format $EDIT_FORMAT) ==="
cp -f "$INPUT" "$TARGET"
code_lines() { grep -vE '^\s*(\*|/\*\*|\*/)' "$1" | grep -v '^\s*$'; }
code_lines "$TARGET" > "$EXP/.code.before"
start=$SECONDS

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
elapsed=$((SECONDS - start))

cp -f "$TARGET" "$EXP/Try.scala.$LABEL"

markers=$(grep -c "TODO FILL IN" "$TARGET" 2>/dev/null || true)
code_lines "$TARGET" > "$EXP/.code.after"
if diff -q "$EXP/.code.before" "$EXP/.code.after" >/dev/null; then integrity=PASS; else integrity=FAIL; fi
classes=$(grep -cE '^final case class (Failure|Success)' "$TARGET" 2>/dev/null || true)

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
  echo "label:            $LABEL"
  echo "model:            $MODEL   (edit-format $EDIT_FORMAT)"
  echo "exit:             $rc      elapsed: ${elapsed}s"
  echo "markers left:     $markers   (0 = fully filled; input had 128)"
  echo "CODE INTEGRITY:   $integrity"
  echo "Failure+Success:  $classes/2 classes present"
  echo "redundant @return:$redundant"
} | tee "$EXP/$LABEL.result.txt"
