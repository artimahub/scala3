#!/usr/bin/env bash
#
# =============================================================================
# run-missing-doc-todos.sh
#
# Drives the "missing documentation" run, one reviewable PR-sized partition at
# a time. For a partition it:
#
#   1. runs todo-writer with --only-undocumented over the partition's generate
#      scope, inserting `TODO FILL IN` Scaladoc stubs ONLY for declarations that
#      have no Scaladoc block at all (already-documented declarations are left
#      alone -- those were handled by the earlier @param/@tparam/@return runs);
#   2. narrows the inserted markers to the partition's keep scope by reverting
#      any changed file that falls outside it (the same "run broad, revert the
#      rest" workaround used by run-cleanup-todos.sh for root files and for
#      collection-core's excluded subtrees);
#   3. calls fill-doc-todos.sh on the remaining marked files, which fills each
#      stub with a Writer (Opus) / Accuracy (Codex) + Style (Sonnet) review loop.
#
# The result is left UNCOMMITTED in the working tree for you to review and turn
# into one PR. Process one partition, review, commit, then run the next.
#
# This mirrors the directory split documented in docs/pr-directory-breakdown.md
# (the "Undocumented batch"), with collection and the loose root files split
# further because brand-new prose is heavier to review than tag-only additions.
#
# Usage:
#   ./run-missing-doc-todos.sh --list                 # show partitions
#   ./run-missing-doc-todos.sh --plan <partition>     # preview target files, then revert (no AI)
#   ./run-missing-doc-todos.sh <partition>            # generate + fill (leaves changes in tree)
#
# Env overrides (forwarded to fill-doc-todos.sh): MAX_ROUNDS, WRITER_MODEL, STYLE_MODEL
# =============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TODO_WRITER_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
REPO_ROOT="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
FILL="$SCRIPT_DIR/fill-doc-todos.sh"
MARKER="TODO FILL IN"

# ---- Partition table --------------------------------------------------------
# Each partition defines two arrays:
#   GEN  -- repo-relative directories to run todo-writer on (it recurses).
#   KEEP -- git pathspecs selecting which changed files belong to this PR.
#           Anything changed under GEN but not matched by KEEP is reverted.
#           Supports git exclude magic, e.g. ':(exclude)<path>', and ':(glob)'
#           so '*' does not cross '/' (used to keep ONLY loose root files).
#
# Order below is the suggested run order (smallest / least entangled first).
partition_spec() {
  local L=library/src/scala
  local LJS=library-js/src/scala
  case "$1" in
    io-ref)
      GEN=("$L/io" "$L/ref")
      KEEP=("$L/io" "$L/ref") ;;
    misc-dirs)   # reflect + small annotation-family dirs (prior PR #11 dirs)
      GEN=("$L/reflect" "$L/annotation" "$L/caps" "$L/deriving" "$L/compat" "$L/beans")
      KEEP=("$L/reflect" "$L/annotation" "$L/caps" "$L/deriving" "$L/compat" "$L/beans") ;;
    sys)
      GEN=("$L/sys"); KEEP=("$L/sys") ;;
    util)
      GEN=("$L/util"); KEEP=("$L/util") ;;
    concurrent)
      GEN=("$L/concurrent"); KEEP=("$L/concurrent") ;;
    math)
      GEN=("$L/math"); KEEP=("$L/math") ;;
    jdk)
      GEN=("$L/jdk"); KEEP=("$L/jdk") ;;
    quoted)
      GEN=("$L/quoted" "$L/compiletime"); KEEP=("$L/quoted" "$L/compiletime") ;;
    runtime)
      GEN=("$L/runtime"); KEEP=("$L/runtime") ;;

    coll-generic)   # generic + concurrent (smallest collection slices)
      GEN=("$L/collection/generic" "$L/collection/concurrent")
      KEEP=("$L/collection/generic" "$L/collection/concurrent") ;;
    coll-convert)
      GEN=("$L/collection/convert"); KEEP=("$L/collection/convert") ;;
    coll-core)      # collection top-level only, excluding the big subtrees
      GEN=("$L/collection")
      KEEP=("$L/collection"
            ":(exclude)$L/collection/mutable"
            ":(exclude)$L/collection/immutable"
            ":(exclude)$L/collection/convert"
            ":(exclude)$L/collection/generic"
            ":(exclude)$L/collection/concurrent") ;;
    coll-mutable)
      GEN=("$L/collection/mutable"); KEEP=("$L/collection/mutable") ;;
    coll-immutable)
      GEN=("$L/collection/immutable"); KEEP=("$L/collection/immutable") ;;

    root-numeric)   # prior PR #5
      GEN=("$L")
      KEEP=("$L/Boolean.scala" "$L/Byte.scala" "$L/Char.scala" "$L/Double.scala"
            "$L/Float.scala" "$L/Int.scala" "$L/Long.scala" "$L/Short.scala") ;;
    root-array-predef)   # prior PR #6
      GEN=("$L")
      KEEP=("$L/Array.scala" "$L/IArray.scala" "$L/Option.scala" "$L/Predef.scala") ;;
    root-function-tuple)   # prior PR #12
      GEN=("$L" "$L/runtime")
      KEEP=(":(glob)$L/Function*.scala" ":(glob)$L/Tuple*.scala" ":(glob)$L/Product*.scala"
            ":(glob)$L/runtime/AbstractFunction*.scala") ;;
    root-misc)   # all remaining loose root files (prior PR #11 root files + stragglers)
      GEN=("$L")
      KEEP=(":(glob)$L/*.scala"
            ":(exclude):(glob)$L/Boolean.scala" ":(exclude):(glob)$L/Byte.scala"
            ":(exclude):(glob)$L/Char.scala" ":(exclude):(glob)$L/Double.scala"
            ":(exclude):(glob)$L/Float.scala" ":(exclude):(glob)$L/Int.scala"
            ":(exclude):(glob)$L/Long.scala" ":(exclude):(glob)$L/Short.scala"
            ":(exclude):(glob)$L/Array.scala" ":(exclude):(glob)$L/IArray.scala"
            ":(exclude):(glob)$L/Option.scala" ":(exclude):(glob)$L/Predef.scala"
            ":(exclude):(glob)$L/Function*.scala" ":(exclude):(glob)$L/Tuple*.scala"
            ":(exclude):(glob)$L/Product*.scala") ;;

    library-js)
      GEN=("$LJS"); KEEP=("$LJS") ;;

    *)
      echo "Unknown partition: $1" >&2
      echo "Run '$(basename "$0") --list' to see valid partitions." >&2
      return 2 ;;
  esac
}

PARTITIONS=(
  io-ref misc-dirs sys util concurrent math jdk quoted runtime
  coll-generic coll-convert coll-core coll-mutable coll-immutable
  root-numeric root-array-predef root-function-tuple root-misc
  library-js
)

list_partitions() {
  echo "Partitions (suggested run order; one PR each):"
  for p in "${PARTITIONS[@]}"; do
    partition_spec "$p"
    printf "  %-20s gen: %s\n" "$p" "${GEN[*]}"
  done
  echo
  echo "Usage: $(basename "$0") [--list | --plan <partition> | <partition>]"
}

# Generate markers over the partition's GEN dirs, then revert anything outside KEEP.
# Echoes the repo-relative target files (those still holding a marker) to stdout.
prepare_partition() {
  local p="$1"
  partition_spec "$p"

  for d in "${GEN[@]}"; do
    echo ">>> todo-writer --only-undocumented on $d" >&2
    ( cd "$TODO_WRITER_DIR" && sbt -batch "run ../$d --only-undocumented" >&2 )
  done

  # Files changed under the generate scope, and the subset we want to keep.
  local changed keep
  changed="$(git -C "$REPO_ROOT" diff --name-only -- "${GEN[@]}")"
  keep="$(git -C "$REPO_ROOT" diff --name-only -- "${KEEP[@]}")"

  # Revert changed files that are not in the keep set.
  local revert
  revert="$(comm -23 <(echo "$changed" | sort -u) <(echo "$keep" | sort -u) | grep -c . || true)"
  if [ "${revert:-0}" -gt 0 ]; then
    comm -23 <(echo "$changed" | sort -u) <(echo "$keep" | sort -u) \
      | while read -r f; do [ -n "$f" ] && git -C "$REPO_ROOT" checkout -- "$f"; done
  fi

  # Emit kept files that actually contain a marker (skip any that didn't change).
  echo "$keep" | while read -r f; do
    [ -n "$f" ] && grep -q "$MARKER" "$REPO_ROOT/$f" 2>/dev/null && echo "$f"
  done
}

main() {
  case "${1:-}" in
    ""|--list|-l|-h|--help)
      list_partitions; exit 0 ;;
    --plan)
      [ $# -ge 2 ] || { echo "--plan needs a partition name" >&2; exit 2; }
      mapfile -t targets < <(prepare_partition "$2")
      echo
      echo "Partition '$2': ${#targets[@]} file(s) with markers:"
      local total=0
      for f in "${targets[@]}"; do
        n=$(grep -cE '/\*\* '"$MARKER" "$REPO_ROOT/$f" 2>/dev/null || echo 0)
        total=$((total + n))
        printf "  %4d decls  %s\n" "$n" "$f"
      done
      echo "  ----"
      printf "  %4d declarations total\n" "$total"
      echo
      echo "Reverting preview changes (no files were filled)."
      for f in "${targets[@]}"; do git -C "$REPO_ROOT" checkout -- "$f"; done
      exit 0 ;;
    *)
      local p="$1"
      mapfile -t targets < <(prepare_partition "$p")
      if [ "${#targets[@]}" -eq 0 ]; then
        echo "No markers generated for partition '$p' -- nothing to fill."
        exit 0
      fi
      echo ">>> filling ${#targets[@]} file(s) for partition '$p'"
      "$FILL" "${targets[@]}"
      echo
      echo "Partition '$p' done. Review with: git -C $REPO_ROOT diff -- ${KEEP[0]} ..."
      echo "Commit as one PR, then run the next partition."
      exit 0 ;;
  esac
}

main "$@"
