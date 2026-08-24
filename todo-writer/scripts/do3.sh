#!/usr/bin/env bash
#
# do3.sh - week 3: put the draft PR branch on top of upstream/main and add the
#          revision as one commit, WITHOUT touching any working tree.
#
# Branch: scaladoc-missing-docs-core-array-function-tuple-sys   (PR #26669)
#
# Why no rebase and no cherry-pick. Both of those have to check files out, and
# in this sandbox that reliably fails partway with
#
#     error: Your local changes to the following files would be overwritten by merge
#
# on a different, arbitrary subset of files each run, even in a fresh worktree
# and even when the tree really is clean. It is a stat-cache race in the
# container's filesystem, not a conflict.
#
# So this builds the two commits the way commit-removals.sh builds its own:
# from tree objects, with git commit-tree. No checkout, no index of yours, no
# working tree, therefore nothing to race. Your checkout is not even read.
#
# It works because no merge is actually needed here:
#
#   - upstream/main and the fable branch's base differ in NONE of the 109
#     week-3 files, so upstream's version of those files IS the base the
#     documentation was written against.
#   - the week-3 commits only ever modify files, never add or delete any.
#
# So the correct result is exactly upstream/main's tree with those 109 files
# swapped for the documented versions. That is a tree overlay, computed in a
# throwaway index, and it is what this script does twice:
#
#   commit 1 = upstream/main + the 109 files as the PR already has them
#              (its own message and authorship preserved)
#   commit 2 = upstream/main + the 109 files as the fable branch has them
#              (the revision plus the audit fixup)
#
# Usage:
#   ./do3.sh          build, verify, print the push command, stop
#   ./do3.sh --push   ... and push

set -euo pipefail

BRANCH=scaladoc-missing-docs-core-array-function-tuple-sys
TARGET=upstream/main
BEFORE=58e044950b                   # the PR's current single commit
PIPELINE=46a297a3d6                 # same documentation, on the fable branch
REVISION=92a8136fbe                 # the revision of it
FIXUP=4199d44413                    # the audit fixup (wrapAccess doc placement)
SOURCE=fable-scaladoc-weeks-3-11    # holds the finished week-3 documentation
EXPECT_FILES=109
EXPECT_REVISED=66

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

origin_sha=$(git rev-parse "origin/$BRANCH")
[ "$(git rev-parse "$BRANCH")" = "$origin_sha" ] || \
  die "local $BRANCH and origin/$BRANCH disagree; reconcile them first"
[ "$(git rev-parse "$BRANCH")" = "$(git rev-parse "$BEFORE")" ] || \
  die "$BRANCH is at $(git rev-parse --short "$BRANCH"), expected $BEFORE"
echo "  $BRANCH = $(git rev-parse --short "$BRANCH")"

# The file list: exactly the files the three week-3 commits touch. Derived
# from the commits themselves rather than from a path pattern, so it cannot
# accidentally pick up a file belonging to another week.
FILES=$(git show --name-only --format= "$PIPELINE" "$REVISION" "$FIXUP" | grep -E '^library' | sort -u)
n_files=$(printf '%s\n' "$FILES" | grep -c . || true)
echo "  week-3 files: $n_files (expected $EXPECT_FILES)"
[ "$n_files" = "$EXPECT_FILES" ] || die "unexpected file count; look before going on"

# Every one of them must exist on the target and be untouched between the
# source branch's base and the target, or an overlay would lose upstream work.
drift=$(git diff --name-only "$(git merge-base "$TARGET" "$SOURCE")" "$TARGET_SHA" -- $(printf '%s ' $FILES) | grep -c . || true)
[ "$drift" = 0 ] || die "$drift week-3 file(s) changed upstream since the docs were written; a plain overlay would discard that. Stop and look."
echo "  upstream changes to those files since the docs were written: 0"

# ---------------------------------------------------------------------------
say "1. safety net"
# ---------------------------------------------------------------------------
BACKUP="backup/${BRANCH}-$(date +%Y%m%d-%H%M%S)"
git branch "$BACKUP" "$BRANCH"
echo "  $BACKUP -> $(git rev-parse --short "$BACKUP")"
echo "  undo with:  git branch -f $BRANCH $BACKUP"

# ---------------------------------------------------------------------------
say "2. build the two trees"
# ---------------------------------------------------------------------------
# A private index in a temp file: your real index is never opened.
TMPIDX=$(mktemp "${TMPDIR:-/tmp}/do3-index.XXXXXX")
trap 'rm -f "$TMPIDX"' EXIT

overlay() {                       # overlay <rev-with-the-file-contents>
  local from=$1
  rm -f "$TMPIDX"
  GIT_INDEX_FILE="$TMPIDX" git read-tree "$TARGET_SHA"
  git ls-tree -r --format='%(objectmode) %(objectname)%x09%(path)' "$from" -- $(printf '%s ' $FILES) \
    | GIT_INDEX_FILE="$TMPIDX" git update-index --index-info
  GIT_INDEX_FILE="$TMPIDX" git write-tree
}

TREE1=$(overlay "$PIPELINE")
TREE2=$(overlay "$SOURCE")
echo "  tree for commit 1: $TREE1"
echo "  tree for commit 2: $TREE2"

# ---------------------------------------------------------------------------
say "3. check the trees before committing to anything"
# ---------------------------------------------------------------------------
MARKS=0
check_tree() {                    # check_tree <tree> <label>; sets $MARKS
  local tree=$1 label=$2 f bad=0 marks=0 n=0
  for f in $FILES; do
    n=$((n+1))
    if [ -n "$(diff <(git show "$TARGET_SHA:$f" | grep -vE '^[[:space:]]*(\*|/\*\*|\*/)' | grep -v '^[[:space:]]*$') \
                    <(git show "$tree:$f"       | grep -vE '^[[:space:]]*(\*|/\*\*|\*/)' | grep -v '^[[:space:]]*$'))" ]; then
      echo "    NOT COMMENT-ONLY: $f"; bad=$((bad+1))
    fi
    local m; m=$(git show "$tree:$f" | grep -ac 'TODO FILL IN' || true)
    [ "$m" != "0" ] && marks=$((marks+m))
  done
  echo "  $label: $n files, code changes: $bad, markers: $marks"
  [ "$bad" = 0 ] || die "$label changes code, not just comments"
  MARKS=$marks
}
check_tree "$TREE1" "commit 1 vs $TARGET"; m1=$MARKS
check_tree "$TREE2" "commit 2 vs $TARGET"; m2=$MARKS
echo "  (commit 1 still carries $m1 unfilled markers; that is the PR as it stands today,"
echo "   and commit 2 is what fills them)"
[ "$m2" = 0 ] || die "the final tree still has $m2 TODO FILL IN marker(s)"

# The final content must match the source branch exactly.
d=0
for f in $FILES; do
  [ "$(git rev-parse "$TREE2:$f")" = "$(git rev-parse "$SOURCE:$f")" ] || { d=$((d+1)); echo "    differs from $SOURCE: $f"; }
done
echo "  final content vs $SOURCE: $d file(s) differ"
[ "$d" = 0 ] || die "the final tree does not match $SOURCE"

# ---------------------------------------------------------------------------
say "4. build the two commits"
# ---------------------------------------------------------------------------
# Commit 1 keeps the original commit's message, author and author date; only
# its parent (and Option.scala, which upstream has since edited) differ.
C1=$(GIT_AUTHOR_NAME=$(git log -1 --format=%an "$BEFORE") \
     GIT_AUTHOR_EMAIL=$(git log -1 --format=%ae "$BEFORE") \
     GIT_AUTHOR_DATE=$(git log -1 --format=%aI "$BEFORE") \
     git commit-tree "$TREE1" -p "$TARGET_SHA" -m "$(git log -1 --format=%B "$BEFORE")")

C2=$(git commit-tree "$TREE2" -p "$C1" -F - <<'MSG'
Revise the added Scaladoc for the core root files, Function/Tuple/Product and sys

Rewrites the documentation added by the first commit on this branch, checking
every claim against the implementation rather than against the name, and fills
the 17 TODO markers left behind in Function7 and AbstractFunction14.

Also moves SystemProperties.wrapAccess's doc comment above its `@nowarn`
annotation. A doc comment below an annotation is dropped by the parser, so
that entry was rendering undocumented.

Still comment-only: no declaration, body, import, annotation or blank line
is touched.
MSG
)
echo "  commit 1: $(git rev-parse --short "$C1")  $(git log -1 --format='%an, %s' "$C1")"
echo "  commit 2: $(git rev-parse --short "$C2")  $(git log -1 --format='%an, %s' "$C2")"
echo "  revision touches $(git diff --name-only "$C1" "$C2" | wc -l | tr -d ' ') files (expected $EXPECT_REVISED)"

# ---------------------------------------------------------------------------
say "5. move the branch"
# ---------------------------------------------------------------------------
git branch -f "$BRANCH" "$C2"
echo "  $BRANCH -> $(git rev-parse --short "$BRANCH"), $(git rev-list --count "$TARGET".."$BRANCH") commits on $TARGET"
echo "  $(git diff --name-only "$TARGET".."$BRANCH" | wc -l | tr -d ' ') files vs $TARGET"
git log --oneline "$TARGET".."$BRANCH" | sed 's/^/    /'

# ---------------------------------------------------------------------------
say "6. push"
# ---------------------------------------------------------------------------
if [ "$DO_PUSH" = yes ]; then
  git push --force-with-lease="$BRANCH:$origin_sha" origin "$BRANCH"
  echo "  pushed."
else
  echo "  Not pushing. When you are ready:"
  echo
  echo "     git push --force-with-lease=$BRANCH:$origin_sha origin $BRANCH"
  echo
  echo "  The lease is against $(git rev-parse --short "$origin_sha"), so the push refuses if"
  echo "  anything else moved the branch meanwhile."
fi

echo
echo "Your working tree was not touched at any point: check with 'git status'."
echo "Undo:  git branch -f $BRANCH $BACKUP"
