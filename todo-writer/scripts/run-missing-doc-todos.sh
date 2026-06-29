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
# A single PR may combine several partitions: pass more than one name and they
# are unioned into one generate+fill pass (used to keep small partitions together
# as one reviewable, ~weekly PR while leaving the big collection partitions solo).
#
# Usage:
#   ./run-missing-doc-todos.sh --list                       # show partitions
#   ./run-missing-doc-todos.sh --plan <partition> [more...] # preview target files, then revert (no AI)
#   ./run-missing-doc-todos.sh <partition> [more...]        # generate + fill (leaves changes in tree)
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
            ":(exclude)$L/Boolean.scala" ":(exclude)$L/Byte.scala"
            ":(exclude)$L/Char.scala" ":(exclude)$L/Double.scala"
            ":(exclude)$L/Float.scala" ":(exclude)$L/Int.scala"
            ":(exclude)$L/Long.scala" ":(exclude)$L/Short.scala"
            ":(exclude)$L/Array.scala" ":(exclude)$L/IArray.scala"
            ":(exclude)$L/Option.scala" ":(exclude)$L/Predef.scala"
            ":(exclude,glob)$L/Function*.scala" ":(exclude,glob)$L/Tuple*.scala"
            ":(exclude,glob)$L/Product*.scala") ;;

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

# Generate markers for one or more partitions (unioned into a single PR), then
# revert anything outside the combined keep set. Echoes the repo-relative target
# files (those still holding a marker) to stdout.
#
# KEEP sets are evaluated PER partition and the resulting file lists unioned, so
# one partition's ':(exclude)' pathspecs never suppress another partition's files.
prepare_partition() {
  local gen_all=() changed="" p
  for p in "$@"; do
    partition_spec "$p"
    gen_all+=("${GEN[@]}")
  done

  # Run todo-writer once per UNIQUE generate dir (combining two root partitions
  # would otherwise scan library/src/scala twice).
  local d
  for d in $(printf '%s\n' "${gen_all[@]}" | sort -u); do
    echo ">>> todo-writer --only-undocumented on $d" >&2
    ( cd "$TODO_WRITER_DIR" && sbt -batch "run ../$d --only-undocumented" >&2 )
  done

  # Now that markers exist, recompute changed + keep across the union.
  changed="$(git -C "$REPO_ROOT" diff --name-only -- "${gen_all[@]}")"
  local keep=""
  for p in "$@"; do
    partition_spec "$p"
    keep+="$(git -C "$REPO_ROOT" diff --name-only -- "${KEEP[@]}")"$'\n'
  done
  keep="$(printf '%s' "$keep" | sort -u | grep .)"

  # Revert changed files that are not in the combined keep set.
  comm -23 <(echo "$changed" | sort -u | grep .) <(echo "$keep" | sort -u | grep .) \
    | while read -r f; do [ -n "$f" ] && git -C "$REPO_ROOT" checkout -- "$f"; done

  # Emit kept files that actually contain a marker.
  echo "$keep" | while read -r f; do
    [ -n "$f" ] && grep -q "$MARKER" "$REPO_ROOT/$f" 2>/dev/null && echo "$f"
  done
}

main() {
  case "${1:-}" in
    ""|--list|-l|-h|--help)
      list_partitions; exit 0 ;;
    --plan)
      shift
      [ $# -ge 1 ] || { echo "--plan needs at least one partition name" >&2; exit 2; }
      mapfile -t targets < <(prepare_partition "$@")
      echo
      echo "PR '$*': ${#targets[@]} file(s) with markers:"
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
      mapfile -t targets < <(prepare_partition "$@")
      if [ "${#targets[@]}" -eq 0 ]; then
        echo "No markers generated for '$*' -- nothing to fill."
        exit 0
      fi
      echo ">>> filling ${#targets[@]} file(s) for PR '$*'"
      "$FILL" "${targets[@]}"
      echo
      echo "PR '$*' done. Review the working tree, then commit as one PR."
      echo "Before the next PR, fold reviewer feedback into docs/house-rules.md."
      exit 0 ;;
  esac
}

main "$@"
