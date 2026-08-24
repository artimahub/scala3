#!/usr/bin/env bash
#
# review-branches.sh - run a Codex review over each open PR branch, weeks 3-12.
#
# For each branch it reviews the WHOLE branch against upstream/main, not just
# the most recent commit, and writes the findings as JSON under
# todo-writer/reviews/branches/. Codex runs read-only: it reads the repository
# for context but changes nothing, and this script writes nothing outside that
# output directory.
#
# The reviews are meant to be adjudicated afterwards (by Opus, in the same way
# the per-file reviews were during the writing run). This script only produces
# them.
#
# CHUNKING. The branch diffs run from 166KB to 453KB. Handing one of those to a
# single call would risk a shallow review at best and a truncated one at worst,
# so each branch is split into chunks of whole files, roughly CHUNK_BYTES of
# diff each, reviewed separately and merged back into one file per branch. The
# per-chunk JSON is kept as well, so nothing is lost if a merge looks wrong.
#
# PAUSING. Before every branch AND every chunk the script checks for a pause
# file. Create it and the script waits where it is, finishing nothing new,
# until you remove it. That is the safe moment to shut the laptop or wait out a
# usage limit.
#
#     touch todo-writer/reviews/branches/PAUSE     # pause at the next boundary
#     rm    todo-writer/reviews/branches/PAUSE     # carry on
#     touch todo-writer/reviews/branches/STOP      # exit cleanly at the next boundary
#
# Work already done is not repeated: a chunk whose JSON exists is skipped, so
# after a STOP (or a crash, or Ctrl-C) just run it again and it picks up where
# it left off. --force reviews everything again.
#
# Usage:
#   ./review-branches.sh                 all ten branches, weeks 3 to 12
#   ./review-branches.sh 9 12            only those weeks
#   ./review-branches.sh --force 9       redo week 9 from scratch
#   ./review-branches.sh --dry-run       show the plan and the chunking, call nothing
#
# Environment:
#   REVIEW_MODEL     model for codex exec         (default: gpt-5.6-terra)
#   CHUNK_BYTES      diff bytes per call          (default: 60000)
#   REVIEW_TIMEOUT   seconds per call             (default: 2400)
#   POLL             pause-file poll interval     (default: 15)

set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

MODEL=${REVIEW_MODEL:-gpt-5.6-terra}
CHUNK_BYTES=${CHUNK_BYTES:-60000}
TIMEOUT=${REVIEW_TIMEOUT:-2400}
POLL=${POLL:-15}

TARGET=upstream/main
OUT=todo-writer/reviews/branches
PROMPT=todo-writer/scripts/prompts/branch-review-prompt.txt
SCHEMA=todo-writer/scripts/schemas/branch-review.schema.json
HOUSE=todo-writer/docs/house-rules.md
PAUSE=$OUT/PAUSE
STOP=$OUT/STOP

FORCE=no
DRY=no
WEEKS=""
for arg in "$@"; do
  case "$arg" in
    --force)   FORCE=yes ;;
    --dry-run) DRY=yes ;;
    *)         WEEKS="$WEEKS $arg" ;;
  esac
done
[ -z "$WEEKS" ] && WEEKS="3 4 5 6 7 8 9 10 11 12"

branch_for() { case "$1" in
  3)  echo scaladoc-missing-docs-core-array-function-tuple-sys ;;
  4)  echo scaladoc-missing-docs-util-concurrent ;;
  5)  echo scaladoc-missing-docs-math-coll-generic ;;
  6)  echo scaladoc-missing-docs-quoted-jdk ;;
  7)  echo scaladoc-missing-docs-collection-convert-js ;;
  8)  echo scaladoc-missing-docs-runtime ;;
  9)  echo scaladoc-missing-docs-collection-mutable ;;
  10) echo scaladoc-missing-docs-collection-core ;;
  11) echo scaladoc-missing-docs-collection-immutable-vector-hashmap-arrayseq ;;
  12) echo scaladoc-missing-docs-collection-immutable-list-lazylist-sorted ;;
esac; }

say()  { printf '\n=== %s\n' "$*"; }
info() { printf '  %s\n' "$*"; }
die()  { printf '\nSTOP: %s\n' "$*" >&2; exit 1; }

for t in codex jq git; do command -v "$t" >/dev/null || die "$t not found on PATH"; done
[ -f "$PROMPT" ] || die "missing $PROMPT"
[ -f "$SCHEMA" ] || die "missing $SCHEMA"
mkdir -p "$OUT"

# Wait here while the pause file exists; leave immediately if asked to stop.
checkpoint() {
  if [ -e "$STOP" ]; then
    printf '\nSTOP file present (%s); exiting cleanly. Re-run to continue.\n' "$STOP"
    exit 0
  fi
  if [ -e "$PAUSE" ]; then
    printf '\n  PAUSED (%s). Nothing new will start. Remove that file to continue.\n' "$PAUSE"
    while [ -e "$PAUSE" ]; do
      if [ -e "$STOP" ]; then
        printf '  STOP file appeared; exiting cleanly.\n'; exit 0
      fi
      sleep "$POLL"
    done
    printf '  resumed\n'
  fi
}

git fetch -q upstream || die "could not fetch upstream"
git fetch -q origin  || true
TARGET_SHA=$(git rev-parse "$TARGET")
say "setup"
info "model:      $MODEL"
info "target:     $TARGET at $(git rev-parse --short "$TARGET")"
info "output:     $OUT"
info "chunk size: $CHUNK_BYTES bytes of diff"
info "weeks:     $WEEKS"
[ "$DRY" = yes ] && info "DRY RUN: no calls will be made"

TMP=$(mktemp -d "${TMPDIR:-/tmp}/review-branches.XXXXXX")
trap 'rm -rf "$TMP"' EXIT

for wk in $WEEKS; do
  BRANCH=$(branch_for "$wk")
  [ -n "$BRANCH" ] || die "unknown week: $wk"
  REF=$BRANCH
  git rev-parse --verify -q "$REF" >/dev/null 2>&1 || REF="origin/$BRANCH"
  git rev-parse --verify -q "$REF" >/dev/null 2>&1 || { info "week $wk: no branch $BRANCH; skipping"; continue; }

  checkpoint

  BASE=$(git merge-base "$TARGET_SHA" "$REF")
  HEAD_SHA=$(git rev-parse "$REF")
  MERGED=$OUT/wk$(printf '%02d' "$wk")-$BRANCH.review.json

  say "week $wk: $BRANCH"
  info "$(git rev-parse --short "$BASE") .. $(git rev-parse --short "$HEAD_SHA")  ($(git rev-list --count "$BASE".."$REF") commit(s))"

  if [ -f "$MERGED" ] && [ "$FORCE" = no ]; then
    info "already reviewed: $MERGED  (use --force to redo)"
    continue
  fi

  # Split the changed files into chunks of about CHUNK_BYTES of diff each.
  rm -f "$TMP"/chunk.*
  n=0; acc=0; chunk=0
  for f in $(git diff --name-only "$BASE" "$REF"); do
    sz=$(git diff "$BASE" "$REF" -- "$f" | wc -c | tr -d ' ')
    if [ "$chunk" = 0 ] || { [ "$acc" -gt 0 ] && [ $((acc + sz)) -gt "$CHUNK_BYTES" ]; }; then
      chunk=$((chunk + 1)); acc=0; : > "$TMP/chunk.$chunk"
    fi
    echo "$f" >> "$TMP/chunk.$chunk"
    acc=$((acc + sz)); n=$((n + 1))
  done
  info "$n files in $chunk chunk(s)"
  [ "$chunk" -gt 0 ] || { info "nothing to review"; continue; }

  for k in $(seq 1 "$chunk"); do
    CJSON=$OUT/wk$(printf '%02d' "$wk")-$BRANCH.chunk$k.json
    CLOG=$OUT/wk$(printf '%02d' "$wk")-$BRANCH.chunk$k.log
    files=$(cat "$TMP/chunk.$k")
    nf=$(printf '%s\n' "$files" | grep -c .)
    bytes=$(git diff "$BASE" "$REF" -- $(printf '%s ' $files) | wc -c | tr -d ' ')

    if [ -s "$CJSON" ] && jq -e . "$CJSON" >/dev/null 2>&1 && [ "$FORCE" = no ]; then
      info "chunk $k/$chunk: already done ($nf files)"
      continue
    fi

    checkpoint
    info "chunk $k/$chunk: $nf file(s), ${bytes} bytes of diff"
    if [ "$DRY" = yes ]; then
      printf '%s\n' "$files" | sed 's/^/      /'
      continue
    fi

    {
      cat "$PROMPT"
      if [ -f "$HOUSE" ]; then
        echo; echo "=== HOUSE RULES (conventions this project has already agreed) ==="
        cat "$HOUSE"
      fi
      echo; echo "=== PULL REQUEST ==="
      echo "branch: $BRANCH (week $wk)"
      echo "base:   $(git rev-parse "$BASE")"
      echo "head:   $HEAD_SHA"
      echo "this is chunk $k of $chunk for this branch"
      echo; echo "=== FILES IN THIS CHUNK ==="
      printf '%s\n' "$files"
      echo; echo "=== DIFF (review only the added and changed comment lines below) ==="
      git diff "$BASE" "$REF" -- $(printf '%s ' $files)
    } > "$TMP/prompt.$k"

    start=$(date +%s)
    set +e
    timeout "$TIMEOUT" codex exec \
      --model "$MODEL" \
      -s read-only \
      --skip-git-repo-check \
      -C "$(pwd)" \
      --output-schema "$SCHEMA" \
      --output-last-message "$CJSON" \
      - < "$TMP/prompt.$k" > "$CLOG" 2>&1
    rc=$?
    set -e
    took=$(( $(date +%s) - start ))

    if [ "$rc" = 124 ]; then
      info "  TIMED OUT after ${TIMEOUT}s; see $CLOG"
      rm -f "$CJSON"
    elif [ "$rc" != 0 ] || [ ! -s "$CJSON" ] || ! jq -e . "$CJSON" >/dev/null 2>&1; then
      info "  FAILED (rc=$rc, ${took}s); see $CLOG"
      rm -f "$CJSON"
    else
      v=$(jq -r '.verdict // "?"' "$CJSON")
      b=$(jq '[.items[]? | select(.severity=="blocker")] | length' "$CJSON")
      nit=$(jq '[.items[]? | select(.severity=="nit")] | length' "$CJSON")
      bon=$(jq '.bonus_findings | length' "$CJSON")
      info "  $v in ${took}s: $b blocker(s), $nit nit(s), $bon bonus"
    fi
  done

  # Merge this branch's chunks into one file.
  parts=$(ls "$OUT"/wk$(printf '%02d' "$wk")-$BRANCH.chunk*.json 2>/dev/null || true)
  if [ -z "$parts" ]; then
    info "no chunk results to merge"
    continue
  fi
  # shellcheck disable=SC2086
  jq -s --arg branch "$BRANCH" --arg week "$wk" --arg base "$(git rev-parse "$BASE")" \
        --arg head "$HEAD_SHA" --arg model "$MODEL" '
    {
      week: $week, branch: $branch, base: $base, head: $head, model: $model,
      chunks: length,
      verdict: (if any(.[]; .verdict == "revise") then "revise" else "approve" end),
      blockers: ([.[].items[]? | select(.severity == "blocker")] | length),
      nits:     ([.[].items[]? | select(.severity == "nit")] | length),
      needs_human: ([.[].items[]? | select(.needs_human)] | length),
      summary: (map(.summary) | join("  |  ")),
      items: (map(.items // []) | add // []),
      bonus_findings: (map(.bonus_findings // []) | add // [])
    }' $parts > "$MERGED"
  info "merged -> $MERGED"
  jq -r '"  verdict: \(.verdict), blockers: \(.blockers), nits: \(.nits), needs_human: \(.needs_human), bonus: \(.bonus_findings|length)"' "$MERGED"
done

# ---------------------------------------------------------------------------
if [ "$DRY" = yes ]; then
  say "dry run complete; nothing was called and nothing was written"
  exit 0
fi

say "index"
# ---------------------------------------------------------------------------
{
  echo "# Codex branch reviews"
  echo
  echo "Generated by \`todo-writer/scripts/review-branches.sh\`, model \`$MODEL\`,"
  echo "against \`$TARGET\` at \`$(git rev-parse --short "$TARGET_SHA")\`."
  echo
  echo "| week | branch | verdict | blockers | nits | needs human | bonus |"
  echo "|---|---|---|---|---|---|---|"
  for f in "$OUT"/*.review.json; do
    [ -e "$f" ] || continue
    jq -r '"| \(.week) | `\(.branch)` | \(.verdict) | \(.blockers) | \(.nits) | \(.needs_human) | \(.bonus_findings|length) |"' "$f"
  done
} > "$OUT/INDEX.md"
cat "$OUT/INDEX.md"
echo
echo "Per-branch JSON: $OUT/wkNN-<branch>.review.json"
echo "Per-chunk JSON and logs kept alongside them."
