#!/usr/bin/env bash
#
# do7through12.sh - weeks 7, 8, 9, 10, 11 and 12 in one run.
#
# (What the project called week 11a and 11b are just weeks 11 and 12 here and
# in the pull requests: collection.immutable was too big for one review, and
# the split is an implementation detail of ours, not something a reviewer
# needs to be told about. The branch names say what is in them instead.)
#
# Creates six new branches, each one commit on top of upstream/main:
#
#   7    scaladoc-missing-docs-collection-convert-js      40 files
#   8    scaladoc-missing-docs-runtime                   119 files
#   9    scaladoc-missing-docs-collection-mutable         40 files
#   10   scaladoc-missing-docs-collection-core            36 files
#   11   scaladoc-missing-docs-collection-immutable-vector-hashmap-arrayseq    5 files
#   12   scaladoc-missing-docs-collection-immutable-list-lazylist-sorted     29 files
#
# Unlike weeks 3 to 6 there is nothing to build on: no PR branch, no working
# branch, nothing but the fable branch itself. I searched every local and
# remote ref by content, not by name; the only other ref holding any of this
# work is marker-backup, the safety snapshot, which stays untouched. So each
# week is one commit, not a first pass plus a revision: all of it was written
# in a single pass, and there is no earlier attempt worth showing separately.
#
# Same technique as do3 through do6: commits built from tree objects with git
# commit-tree, because rebase and cherry-pick both hit this sandbox's
# stat-cache race. No checkout, no index of yours, no working tree.
#
# An overlay is exact here for the same two reasons as before, re-checked per
# week at run time: upstream has changed none of these files since the
# documentation was written, and the work only ever modifies files, never adds
# or deletes one.
#
# A week whose branch already exists is skipped with a warning rather than
# clobbered, so a partial re-run is safe.
#
# Usage:
#   ./do7through11b.sh          build all six, verify, print push commands
#   ./do7through11b.sh --push   ... and push them
#   ./do7through11b.sh 9 12     just those weeks

set -euo pipefail

TARGET=upstream/main
SOURCE=fable-scaladoc-weeks-3-11
ALL_WEEKS="7 8 9 10 11 12"

DO_PUSH=no
WEEKS=""
for arg in "$@"; do
  case "$arg" in
    --push) DO_PUSH=yes ;;
    *)      WEEKS="$WEEKS $arg" ;;
  esac
done
[ -z "$WEEKS" ] && WEEKS=$ALL_WEEKS

say()  { printf '\n=== %s\n' "$*"; }
warn() { printf '  !! %s\n' "$*"; }
die()  { printf '\nSTOP: %s\n' "$*" >&2; exit 1; }

branch_for() { case "$1" in
  7)   echo scaladoc-missing-docs-collection-convert-js ;;
  8)   echo scaladoc-missing-docs-runtime ;;
  9)   echo scaladoc-missing-docs-collection-mutable ;;
  10)  echo scaladoc-missing-docs-collection-core ;;
  11)  echo scaladoc-missing-docs-collection-immutable-vector-hashmap-arrayseq ;;
  12)  echo scaladoc-missing-docs-collection-immutable-list-lazylist-sorted ;;
esac; }

commits_for() { case "$1" in
  7)   echo f19cbe30fb ;;
  8)   echo 5120376880 ;;
  9)   echo "bbaa29ed41 28afc16801 23dc7b1f04" ;;   # week 9 + two audit fixups
  10)  echo 6b5cbdefb0 ;;
  11)  echo c2a4e6d376 ;;
  12)  echo "8c78126fba 826ff0e4b5 92f203eef4 69deef6cf6" ;;
esac; }

expect_for() { case "$1" in
  7) echo 40 ;; 8) echo 119 ;; 9) echo 40 ;; 10) echo 36 ;; 11) echo 5 ;; 12) echo 29 ;;
esac; }

message_for() { case "$1" in
7) cat <<'MSG'
Add Scaladoc for undocumented collection.convert APIs and the Scala.js library

Documents declarations that had no doc comment at all in
scala.collection.convert, most of them in the stepper implementations under
convert.impl, together with the Scala.js variants of files whose JVM
counterparts are documented elsewhere in this series.

Comment-only: no declaration, body, import, annotation or blank line is
touched.
MSG
;;
8) cat <<'MSG'
Add Scaladoc for undocumented scala.runtime APIs

Documents declarations that had no doc comment at all across scala.runtime:
the boxed and unboxed value-class runtime support, the array and tuple
helpers, and the 87 files of scala.runtime.java8 that back Java function
interop.

Comment-only: no declaration, body, import, annotation or blank line is
touched.
MSG
;;
9) cat <<'MSG'
Add Scaladoc for undocumented scala.collection.mutable APIs

Documents declarations that had no doc comment at all across
scala.collection.mutable: the buffers and array-backed sequences, the hash
and tree maps and sets, the builders, and the specialised maps.

Includes two placement corrections found while auditing: doc comments that
had been written below an annotation, where the parser drops them, and one
written inside a commented-out block.

Comment-only: no declaration, body, import, annotation or blank line is
touched.
MSG
;;
10) cat <<'MSG'
Add Scaladoc for undocumented scala.collection APIs

Documents declarations that had no doc comment at all in the root of
scala.collection: the Iterable, Seq, Set and Map hierarchies and their Ops
traits, the views, the steppers and the factory machinery.

Comment-only: no declaration, body, import, annotation or blank line is
touched.
MSG
;;
11) cat <<'MSG'
Add Scaladoc for undocumented immutable Vector, HashMap, HashSet, ArraySeq and Map

Documents declarations that had no doc comment at all in the indexed and
hashed collections of scala.collection.immutable: Vector and its builder,
ArraySeq and its specialised subclasses, HashMap and HashSet including the
CHAMP trie internals that back them, and Map with its small-arity Map1 to
Map4 family.

Comment-only: no declaration, body, import, annotation or blank line is
touched.
MSG
;;
12) cat <<'MSG'
Add Scaladoc for undocumented immutable List, LazyList, and the sorted and tree collections

Documents declarations that had no doc comment at all in the linear, lazy and
ordered collections of scala.collection.immutable: List and Set with the
small-arity Set1 to Set4 family, the lazy sequences LazyList, LazyListIterable
and the deprecated Stream, the red-black tree maps and sets, the sorted and
list-backed maps and sets, Queue, WrappedString, the ranges, and the
integer-keyed maps.

Comment-only: no declaration, body, import, annotation or blank line is
touched.
MSG
;;
esac; }

# ---------------------------------------------------------------------------
say "preflight"
# ---------------------------------------------------------------------------
cd "$(git rev-parse --show-toplevel)"
git fetch -q upstream
git fetch -q origin
TARGET_SHA=$(git rev-parse "$TARGET")
BASE=$(git merge-base "$TARGET" "$SOURCE")
echo "  $TARGET = $(git rev-parse --short "$TARGET")  ($(git log -1 --format=%ad --date=short "$TARGET"))"
echo "  $SOURCE = $(git rev-parse --short "$SOURCE")"
echo "  weeks to build:$WEEKS"

TMPIDX=$(mktemp "${TMPDIR:-/tmp}/do7-index.XXXXXX")
trap 'rm -f "$TMPIDX"' EXIT

CREATED=""
SKIPPED=""

for wk in $WEEKS; do
  BRANCH=$(branch_for "$wk")
  [ -n "$BRANCH" ] || die "unknown week: $wk"
  COMMITS=$(commits_for "$wk")
  EXPECT=$(expect_for "$wk")

  say "week $wk -> $BRANCH"

  if git rev-parse --verify -q "$BRANCH" >/dev/null 2>&1; then
    warn "local branch $BRANCH already exists; skipping this week"
    SKIPPED="$SKIPPED $wk"; continue
  fi
  if git rev-parse --verify -q "origin/$BRANCH" >/dev/null 2>&1; then
    warn "origin/$BRANCH already exists; skipping this week"
    SKIPPED="$SKIPPED $wk"; continue
  fi

  FILES=$(git show --name-only --format= $COMMITS | grep -E '^library' | sort -u)
  n_files=$(printf '%s\n' "$FILES" | grep -c .)
  echo "  files: $n_files (expected $EXPECT), from $(printf '%s' "$COMMITS" | wc -w | tr -d ' ') commit(s)"
  [ "$n_files" = "$EXPECT" ] || die "week $wk: expected $EXPECT files, found $n_files"

  for f in $FILES; do
    git cat-file -e "$TARGET_SHA:$f" 2>/dev/null || die "week $wk: $f does not exist on $TARGET"
  done

  drift=$(git diff --name-only "$BASE" "$TARGET_SHA" -- $(printf '%s ' $FILES) | grep -c . || true)
  [ "$drift" = 0 ] || die "week $wk: $drift file(s) changed upstream since the docs were written; an overlay would discard that. Stop and look."
  echo "  upstream changes to them since the docs were written: 0"

  # tree = upstream/main with these files taken from the source branch
  rm -f "$TMPIDX"
  GIT_INDEX_FILE="$TMPIDX" git read-tree "$TARGET_SHA"
  git ls-tree -r --format='%(objectmode) %(objectname)%x09%(path)' "$SOURCE" -- $(printf '%s ' $FILES) \
    | GIT_INDEX_FILE="$TMPIDX" git update-index --index-info
  TREE=$(GIT_INDEX_FILE="$TMPIDX" git write-tree)

  bad=0; marks=0
  for f in $FILES; do
    if [ -n "$(diff <(git show "$TARGET_SHA:$f" | grep -vE '^[[:space:]]*(\*|/\*\*|\*/)' | grep -v '^[[:space:]]*$') \
                    <(git show "$TREE:$f"       | grep -vE '^[[:space:]]*(\*|/\*\*|\*/)' | grep -v '^[[:space:]]*$'))" ]; then
      echo "    NOT COMMENT-ONLY: $f"; bad=$((bad+1))
    fi
    m=$(git show "$TREE:$f" | grep -ac 'TODO FILL IN' || true)
    [ "$m" != "0" ] && { echo "    MARKERS ($m): $f"; marks=$((marks+m)); }
  done
  echo "  code changes: $bad, markers: $marks"
  [ "$bad" = 0 ] || die "week $wk changes code, not just comments"
  [ "$marks" = 0 ] || die "week $wk still has $marks TODO FILL IN marker(s)"

  d=0
  for f in $FILES; do
    [ "$(git rev-parse "$TREE:$f")" = "$(git rev-parse "$SOURCE:$f")" ] || d=$((d+1))
  done
  [ "$d" = 0 ] || die "week $wk: $d file(s) do not match $SOURCE"
  echo "  content matches $SOURCE in all $n_files files"

  C=$(message_for "$wk" | git commit-tree "$TREE" -p "$TARGET_SHA")
  git branch "$BRANCH" "$C"
  echo "  created $BRANCH -> $(git rev-parse --short "$BRANCH")"
  echo "    $(git log -1 --format=%s "$C")"
  CREATED="$CREATED $wk"
done

# ---------------------------------------------------------------------------
say "summary"
# ---------------------------------------------------------------------------
echo "  created:$([ -n "$CREATED" ] && echo "$CREATED" || echo ' none')"
[ -n "$SKIPPED" ] && echo "  skipped (already existed):$SKIPPED"
for wk in $CREATED; do
  B=$(branch_for "$wk")
  printf '  %-5s %-48s %s commit, %s files\n' "$wk" "$B" \
    "$(git rev-list --count "$TARGET".."$B")" "$(git diff --name-only "$TARGET".."$B" | wc -l | tr -d ' ')"
done

# no file may appear in two of these branches, or the PRs would collide
if [ -n "$CREATED" ]; then
  dup=$(for wk in $CREATED; do git diff --name-only "$TARGET"..$(branch_for "$wk"); done | sort | uniq -d | grep -c . || true)
  echo "  files claimed by more than one branch: $dup"
  [ "$dup" = 0 ] || die "the branches overlap; that must not happen"
fi

say "push"
if [ "$DO_PUSH" = yes ]; then
  for wk in $CREATED; do
    B=$(branch_for "$wk"); git push -u origin "$B"; echo "  pushed $B"
  done
else
  echo "  Not pushing. When you are ready:"
  echo
  for wk in $CREATED; do echo "     git push -u origin $(branch_for "$wk")"; done
  echo
  echo "  All new branches, so no force and no lease are needed."
fi

echo
echo "Your working tree was not touched at any point: check with 'git status'."
echo "Undo:$(for wk in $CREATED; do printf ' %s' "$(branch_for "$wk")"; done | sed 's/^/ git branch -D/')"
