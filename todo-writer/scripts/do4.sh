#!/usr/bin/env bash
#
# do4.sh - week 4: put the draft PR branch on top of upstream/main and add the
#          revision as one commit, WITHOUT touching any working tree.
#
# Branch: scaladoc-missing-docs-util-concurrent   (PR #26822)
#
# Same technique as do3.sh, and for the same reason: rebase and cherry-pick
# both fail in this sandbox with "local changes would be overwritten" on a
# different arbitrary subset of files each run, on a clean tree, in a fresh
# worktree as much as in place. That is a stat-cache race in the container
# filesystem, not a conflict. So the commits are built from tree objects with
# git commit-tree, the way commit-removals.sh does. No checkout, no index of
# yours, no working tree.
#
# One difference from week 3. That branch had a single commit; this one has
# three, and they are worth keeping as they are:
#
#     a18e3332c0  Add Scaladoc for undocumented util and concurrent APIs
#     af4c7bea0c  Address review feedback on util and concurrent Scaladoc
#     690244164c  Address second round of review feedback on ...
#
# The last two are your responses to cheeseng's review, so this replays all
# three onto upstream/main, each keeping its own message, author and author
# date, and then adds the revision as a fourth commit. The PR ends up four
# commits on upstream/main instead of three on a base from 2026-08-15.
#
# Why an overlay is exact here, as it was for week 3:
#
#   - upstream/main and the base the documentation was written against differ
#     in NONE of the 36 week-4 files
#   - neither the branch's commits nor the fable branch's week-4 commits add
#     or delete a file; they only modify
#
# so each replayed commit is just upstream/main's tree with those 36 files
# swapped for that commit's versions. The script re-checks both assumptions
# and refuses to run if either has stopped holding.
#
# Usage:
#   ./do4.sh          build, verify, print the push command, stop
#   ./do4.sh --push   ... and push

set -euo pipefail

BRANCH=scaladoc-missing-docs-util-concurrent
TARGET=upstream/main
OLD_BASE=3adfcbd32a                 # base the branch was cut from, 2026-08-15
BEFORE=690244164c                   # the branch's current tip
PIPELINE=ba8226af63                 # week 4 on the fable branch
REVISION=e7522614bc                 # the revision of it
SOURCE=fable-scaladoc-weeks-3-11    # holds the finished week-4 documentation
EXPECT_FILES=36
EXPECT_COMMITS=3                    # commits already on the branch
EXPECT_REVISED=18                   # files the revision commit touches

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

ORIGINALS=$(git rev-list --reverse "$OLD_BASE..$BRANCH")
n_commits=$(printf '%s\n' "$ORIGINALS" | grep -c .)
echo "  $BRANCH = $(git rev-parse --short "$BRANCH"), $n_commits commit(s) on $OLD_BASE:"
for c in $ORIGINALS; do echo "    $(git rev-parse --short "$c")  $(git log -1 --format=%s "$c")"; done
[ "$n_commits" = "$EXPECT_COMMITS" ] || die "expected $EXPECT_COMMITS commits, found $n_commits"

# The file set: the union of what the fable branch's week-4 commits touch.
FILES=$(git show --name-only --format= "$PIPELINE" "$REVISION" | grep -E '^library' | sort -u)
n_files=$(printf '%s\n' "$FILES" | grep -c .)
echo "  week-4 files: $n_files (expected $EXPECT_FILES)"
[ "$n_files" = "$EXPECT_FILES" ] || die "unexpected file count; look before going on"

# The branch's own commits must stay inside that set, or replaying them as an
# overlay of these files would silently drop something.
outside=$(git diff --name-only "$OLD_BASE" "$BRANCH" | sort -u | comm -23 - <(printf '%s\n' "$FILES") | grep -c . || true)
[ "$outside" = 0 ] || die "the branch touches $outside file(s) outside the week-4 set; stop and look"
echo "  branch touches nothing outside that set"

# The revision's own text was written against the current upstream code, so
# nothing in the set may have moved upstream since then.
drift=$(git diff --name-only "$(git merge-base "$TARGET" "$SOURCE")" "$TARGET_SHA" -- $(printf '%s ' $FILES) | grep -c . || true)
[ "$drift" = 0 ] || die "$drift week-4 file(s) changed upstream since the docs were written; an overlay would discard that. Stop and look."
echo "  upstream changes to those files since the docs were written: 0"

# The branch's OWN commits are a different matter: they were written against a
# 2026-08-15 base, and upstream has edited some of these files since. Replaying
# such a commit as a plain overlay would revert that upstream work, so those
# files get a real 3-way merge instead (in /tmp, not in any working tree).
DRIFTED=$(git diff --name-only "$OLD_BASE" "$TARGET_SHA" -- $(printf '%s ' $FILES) || true)
n_drifted=$(printf '%s\n' "$DRIFTED" | grep -c . || true)
CLEAN_FILES=$(printf '%s\n' "$FILES" | grep -vxF -f <(printf '%s\n' "$DRIFTED") || printf '%s\n' "$FILES")
echo "  files upstream has edited since the branch's base: $n_drifted"
for f in $DRIFTED; do echo "    $f (3-way merged when replaying)"; done

# ---------------------------------------------------------------------------
say "1. safety net"
# ---------------------------------------------------------------------------
BACKUP="backup/${BRANCH}-$(date +%Y%m%d-%H%M%S)"
git branch "$BACKUP" "$BRANCH"
echo "  $BACKUP -> $(git rev-parse --short "$BACKUP")"
echo "  undo with:  git branch -f $BRANCH $BACKUP"

# ---------------------------------------------------------------------------
say "2. build the trees"
# ---------------------------------------------------------------------------
TMPIDX=$(mktemp "${TMPDIR:-/tmp}/do4-index.XXXXXX")
TMPD=$(mktemp -d "${TMPDIR:-/tmp}/do4-merge.XXXXXX")
trap 'rm -f "$TMPIDX"; rm -rf "$TMPD"' EXIT

# overlay <rev> [merge]
#   upstream/main's tree with the week-4 files taken from <rev>.
#   With "merge", any file upstream has edited since $OLD_BASE is 3-way merged
#   (that commit's text, upstream's code) instead of copied wholesale.
overlay() {
  local from=$1 mode=${2:-copy} f blob fmode
  rm -f "$TMPIDX"
  GIT_INDEX_FILE="$TMPIDX" git read-tree "$TARGET_SHA"

  local plain="$FILES"
  [ "$mode" = merge ] && plain="$CLEAN_FILES"
  if [ -n "$plain" ]; then
    git ls-tree -r --format='%(objectmode) %(objectname)%x09%(path)' "$from" -- $(printf '%s ' $plain) \
      | GIT_INDEX_FILE="$TMPIDX" git update-index --index-info
  fi

  if [ "$mode" = merge ]; then
    for f in $DRIFTED; do
      git show "$from:$f"       > "$TMPD/ours"
      git show "$OLD_BASE:$f"   > "$TMPD/base"
      git show "$TARGET_SHA:$f" > "$TMPD/theirs"
      if ! git merge-file -p "$TMPD/ours" "$TMPD/base" "$TMPD/theirs" > "$TMPD/merged"; then
        die "3-way merge of $f conflicted; resolve it by hand"
      fi
      blob=$(git hash-object -w "$TMPD/merged")
      fmode=$(git ls-tree "$from" -- "$f" | awk '{print $1}')
      printf '%s %s\t%s\n' "$fmode" "$blob" "$f" \
        | GIT_INDEX_FILE="$TMPIDX" git update-index --index-info
    done
  fi

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

# ---------------------------------------------------------------------------
say "3. replay the branch's own commits onto $TARGET"
# ---------------------------------------------------------------------------
PARENT=$TARGET_SHA
i=0
for c in $ORIGINALS; do
  i=$((i+1))
  T=$(overlay "$c" merge)
  check_tree "$T" "  commit $i ($(git rev-parse --short "$c")) vs $TARGET"
  NEW=$(GIT_AUTHOR_NAME=$(git log -1 --format=%an "$c") \
        GIT_AUTHOR_EMAIL=$(git log -1 --format=%ae "$c") \
        GIT_AUTHOR_DATE=$(git log -1 --format=%aI "$c") \
        git commit-tree "$T" -p "$PARENT" -m "$(git log -1 --format=%B "$c")")
  echo "    -> $(git rev-parse --short "$NEW")  $(git log -1 --format='%an, %ad' --date=short "$NEW")"
  PARENT=$NEW
done

# ---------------------------------------------------------------------------
say "4. add the revision as one more commit"
# ---------------------------------------------------------------------------
TFINAL=$(overlay "$SOURCE")
check_tree "$TFINAL" "  final tree vs $TARGET"
[ "$MARKS" = 0 ] || die "the final tree still has $MARKS TODO FILL IN marker(s)"

d=0
for f in $FILES; do
  [ "$(git rev-parse "$TFINAL:$f")" = "$(git rev-parse "$SOURCE:$f")" ] || { d=$((d+1)); echo "    differs from $SOURCE: $f"; }
done
echo "  final content vs $SOURCE: $d file(s) differ"
[ "$d" = 0 ] || die "the final tree does not match $SOURCE"

FINAL=$(git commit-tree "$TFINAL" -p "$PARENT" -F - <<'MSG'
Revise the added Scaladoc for scala.util and scala.concurrent

Rechecks every claim in the documentation on this branch against the
implementation rather than against the name, and corrects what did not hold,
notably in Exception, Regex, Properties, Duration, BatchingExecutor and the
Future converters.

Also removes two markers that had been inserted inside the @implicitNotFound
string literal of ExecutionContext, restoring that message to its upstream
text.

Still comment-only: no declaration, body, import, annotation or blank line
is touched.
MSG
)
echo "  revision commit: $(git rev-parse --short "$FINAL")"
echo "  it touches $(git diff --name-only "$PARENT" "$FINAL" | wc -l | tr -d ' ') files (expected $EXPECT_REVISED)"

# ---------------------------------------------------------------------------
say "5. move the branch"
# ---------------------------------------------------------------------------
git branch -f "$BRANCH" "$FINAL"
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
