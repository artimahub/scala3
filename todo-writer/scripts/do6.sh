#!/usr/bin/env bash
#
# do6.sh - week 6: create the PR branch on top of upstream/main, two commits,
#          WITHOUT touching any working tree.
#
# Branch: scaladoc-missing-docs-quoted-jdk   (NEW; nothing on origin yet)
#
# Read this bit before running, because week 6 is not like 3, 4 and 5.
#
# There IS a week-6 branch already, exp/wk6, and it is NOT the branch to build
# on. It is the working branch from the week-6 pipeline run: 17 commits, most
# of them per-file ("Fill in Scaladoc TODOs for jdk.IntAccumulator", "Refine
# ExprMap Scaladoc per adjudicated review", ...), and it touches 569 files
# because it also carries the whole todo-writer/ tooling tree. None of that
# belongs in a pull request to scala/scala3.
#
# What matters is that its library content is exactly the fable branch's
# "Week 6 (partial)" commit, 5ac2a08bc1 - I checked all 17 files, byte for
# byte. So that commit is the clean, library-only squash of everything on
# exp/wk6, and this script uses it as the PR's first commit. Nothing is lost
# by leaving exp/wk6 alone; it stays as the detailed record.
#
# The result is two commits, matching how weeks 3-5 read:
#
#   1. the first pass: scala.jdk plus quoted/Expr and quoted/ExprMap (12 files)
#   2. the completion: the rest of scala.quoted, including Quotes.scala's 285
#      declarations, and a revision of the jdk work (14 of the 17 files change)
#
# Also worth knowing: week 6 was planned as quoted + compiletime + jdk, but
# scala.compiletime turned out to have nothing missing, so the branch is named
# for the two packages actually touched.
#
# Technique is the same as do3-do5: commits built from tree objects with git
# commit-tree, because rebase and cherry-pick both hit this sandbox's
# stat-cache race. No checkout, no index of yours, no working tree.
#
# Usage:
#   ./do6.sh          build, verify, print the push command, stop
#   ./do6.sh --push   ... and push

set -euo pipefail

BRANCH=scaladoc-missing-docs-quoted-jdk
TARGET=upstream/main
FIRST=5ac2a08bc1                    # "Week 6 (partial)" on the fable branch
SECOND=04dca3f758                   # "Week 6 completion"
SOURCE=fable-scaladoc-weeks-3-11    # holds the finished week-6 documentation
EXPECT_FILES=17
EXPECT_FIRST=12                     # files the first commit touches
EXPECT_SECOND=14                    # files the second commit changes

DO_PUSH=no
if [ "${1:-}" = "--push" ]; then DO_PUSH=yes; fi

say() { printf '\n=== %s\n' "$*"; }
die() { printf '\nSTOP: %s\n' "$*" >&2; exit 1; }

# ---------------------------------------------------------------------------
say "0. preflight"
# ---------------------------------------------------------------------------
cd "$(git rev-parse --show-toplevel)"
git fetch -q upstream
git fetch -q origin

TARGET_SHA=$(git rev-parse "$TARGET")
echo "  $TARGET = $(git rev-parse --short "$TARGET")  ($(git log -1 --format=%ad --date=short "$TARGET"))"

# This branch is new. Refuse to clobber anything that already exists.
git rev-parse --verify -q "$BRANCH" >/dev/null 2>&1 && \
  die "local branch $BRANCH already exists; delete it or pick another name"
git rev-parse --verify -q "origin/$BRANCH" >/dev/null 2>&1 && \
  die "origin/$BRANCH already exists; this script only creates a new branch"
echo "  $BRANCH does not exist yet, locally or on origin"

FILES=$(git show --name-only --format= "$FIRST" "$SECOND" | grep -E '^library' | sort -u)
n_files=$(printf '%s\n' "$FILES" | grep -c .)
echo "  week-6 files: $n_files (expected $EXPECT_FILES)"
[ "$n_files" = "$EXPECT_FILES" ] || die "unexpected file count; look before going on"

for f in $FILES; do
  git cat-file -e "$TARGET_SHA:$f" 2>/dev/null || die "$f does not exist on $TARGET"
done
echo "  all of them exist on $TARGET"

# The documentation was written against the fable branch's base. If upstream
# has edited any of these files since, a plain overlay would revert that work.
BASE=$(git merge-base "$TARGET" "$SOURCE")
drift=$(git diff --name-only "$BASE" "$TARGET_SHA" -- $(printf '%s ' $FILES) | grep -c . || true)
[ "$drift" = 0 ] || die "$drift week-6 file(s) changed upstream since the docs were written; a plain overlay would discard that. Stop and look."
echo "  upstream changes to those files since the docs were written: 0"

# exp/wk6's library content must still match $FIRST, or the claim in the
# header above is stale and the first commit would not be what it says it is.
if git rev-parse --verify -q origin/exp/wk6 >/dev/null 2>&1; then
  d=0
  for f in $FILES; do
    [ "$(git rev-parse "origin/exp/wk6:$f" 2>/dev/null || echo x)" = "$(git rev-parse "$FIRST:$f")" ] || d=$((d+1))
  done
  echo "  exp/wk6 library content vs $FIRST: $d of $n_files differ"
  [ "$d" = 0 ] || echo "    (it has moved on; the first commit follows $FIRST, not exp/wk6)"
fi

# ---------------------------------------------------------------------------
say "1. build the trees"
# ---------------------------------------------------------------------------
TMPIDX=$(mktemp "${TMPDIR:-/tmp}/do6-index.XXXXXX")
trap 'rm -f "$TMPIDX"' EXIT

overlay() {                       # overlay <rev-holding-the-file-contents>
  local from=$1
  rm -f "$TMPIDX"
  GIT_INDEX_FILE="$TMPIDX" git read-tree "$TARGET_SHA"
  git ls-tree -r --format='%(objectmode) %(objectname)%x09%(path)' "$from" -- $(printf '%s ' $FILES) \
    | GIT_INDEX_FILE="$TMPIDX" git update-index --index-info
  GIT_INDEX_FILE="$TMPIDX" git write-tree
}

MARKS=0
check_tree() {                    # check_tree <tree> <label>; sets $MARKS
  local tree=$1 label=$2 f bad=0 marks=0 n=0 m
  for f in $FILES; do
    n=$((n+1))
    if [ -n "$(diff <(git show "$TARGET_SHA:$f" | grep -vE '^[[:space:]]*(\*|/\*\*|\*/)' | grep -v '^[[:space:]]*$') \
                    <(git show "$tree:$f"       | grep -vE '^[[:space:]]*(\*|/\*\*|\*/)' | grep -v '^[[:space:]]*$'))" ]; then
      echo "    NOT COMMENT-ONLY: $f"; bad=$((bad+1))
    fi
    m=$(git show "$tree:$f" | grep -ac 'TODO FILL IN' || true)
    [ "$m" != "0" ] && marks=$((marks+m))
  done
  echo "  $label: $n files, code changes: $bad, markers: $marks"
  [ "$bad" = 0 ] || die "$label changes code, not just comments"
  MARKS=$marks
}

TREE1=$(overlay "$FIRST")
TREE2=$(overlay "$SOURCE")
check_tree "$TREE1" "first pass vs $TARGET"
check_tree "$TREE2" "final    vs $TARGET"
[ "$MARKS" = 0 ] || die "the final tree still has $MARKS TODO FILL IN marker(s)"

d=0
for f in $FILES; do
  [ "$(git rev-parse "$TREE2:$f")" = "$(git rev-parse "$SOURCE:$f")" ] || { d=$((d+1)); echo "    differs from $SOURCE: $f"; }
done
echo "  final content vs $SOURCE: $d file(s) differ"
[ "$d" = 0 ] || die "the final tree does not match $SOURCE"

# ---------------------------------------------------------------------------
say "2. build the two commits"
# ---------------------------------------------------------------------------
C1=$(GIT_AUTHOR_NAME=$(git log -1 --format=%an "$FIRST") \
     GIT_AUTHOR_EMAIL=$(git log -1 --format=%ae "$FIRST") \
     GIT_AUTHOR_DATE=$(git log -1 --format=%aI "$FIRST") \
     git commit-tree "$TREE1" -p "$TARGET_SHA" -F - <<'MSG'
Add Scaladoc for undocumented scala.jdk APIs and two scala.quoted files

Documents declarations that had no doc comment at all across scala.jdk (the
four accumulators and their shared base, the function wrappers and
extensions, the duration and future converters, OptionShape) and the first
two files of scala.quoted, Expr and ExprMap.

Comment-only: no declaration, body, import, annotation or blank line is
touched.
MSG
)

C2=$(git commit-tree "$TREE2" -p "$C1" -F - <<'MSG'
Complete the Scaladoc for scala.quoted and revise scala.jdk

Documents the rest of scala.quoted, most of it Quotes.scala, which alone
accounts for 285 newly documented declarations across the reflection API,
along with FromExpr, Type, QuoteMatching and StopMacroExpansion.

Also revises the scala.jdk documentation added by the previous commit,
rechecking each claim against the implementation rather than against the
name: the accumulator steppers in particular describe what their guards
actually do.

Comment-only: no declaration, body, import, annotation or blank line is
touched.
MSG
)
echo "  commit 1: $(git rev-parse --short "$C1")  $(git log -1 --format='%an, %ad' --date=short "$C1")"
echo "  commit 2: $(git rev-parse --short "$C2")"
n1=$(git diff --name-only "$TARGET_SHA" "$C1" | wc -l | tr -d ' ')
n2=$(git diff --name-only "$C1" "$C2" | wc -l | tr -d ' ')
echo "  commit 1 touches $n1 files (expected $EXPECT_FIRST), commit 2 changes $n2 (expected $EXPECT_SECOND)"
[ "$n1" = "$EXPECT_FIRST" ] || die "commit 1 touches $n1 files, expected $EXPECT_FIRST"
[ "$n2" = "$EXPECT_SECOND" ] || die "commit 2 changes $n2 files, expected $EXPECT_SECOND"

# ---------------------------------------------------------------------------
say "3. create the branch"
# ---------------------------------------------------------------------------
git branch "$BRANCH" "$C2"
echo "  $BRANCH -> $(git rev-parse --short "$BRANCH"), $(git rev-list --count "$TARGET".."$BRANCH") commits on $TARGET"
echo "  $(git diff --name-only "$TARGET".."$BRANCH" | wc -l | tr -d ' ') files vs $TARGET"
git log --oneline "$TARGET".."$BRANCH" | sed 's/^/    /'

# ---------------------------------------------------------------------------
say "4. push"
# ---------------------------------------------------------------------------
if [ "$DO_PUSH" = yes ]; then
  git push -u origin "$BRANCH"
  echo "  pushed."
else
  echo "  Not pushing. When you are ready:"
  echo
  echo "     git push -u origin $BRANCH"
  echo
  echo "  It is a new branch, so no force and no lease are needed."
fi

echo
echo "Your working tree was not touched at any point: check with 'git status'."
echo "Undo:  git branch -D $BRANCH"
