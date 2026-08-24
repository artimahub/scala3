#!/usr/bin/env bash
#
# do3.sh - week 3 only: put the draft PR branch on top of upstream/main, then
#          add the revision as one commit on top.
#
# Branch: scaladoc-missing-docs-core-array-function-tuple-sys   (PR #26669)
#
# Order matters, and this script does it in the order I recommend:
#
#   1. rebase the branch's existing commit onto upstream/main
#   2. THEN cherry-pick the revision + audit fixup as one new commit
#
# Why that order. Both orders end up byte-identical (I tested both against
# today's tree), but this one is easier to live with:
#
#   - Two pushes with different characters instead of one muddled push. The
#     rebase changes the base and nothing else; the cherry-pick changes
#     documentation and nothing else. If something looks wrong afterwards you
#     know which step did it, and the PR timeline shows reviewers "1 new
#     commit" rather than a wholesale rewrite.
#   - The cherry-pick lands against current upstream code, so if it ever did
#     conflict you would resolve it once, in the context that ships. Do it the
#     other way round and the later rebase replays BOTH commits, so the same
#     conflict can come back a second time.
#
# For week 3 specifically the rebase is nearly a no-op: of the 109 files in
# this PR, exactly one (Option.scala) was touched upstream since the branch
# was cut, and none were touched between the fable base and upstream/main.
#
# This script does NOT push unless you ask it to. It stops after verifying and
# prints the exact push command.
#
# Usage:
#   ./do3.sh                    both steps, verify, stop before pushing
#   ./do3.sh --push             both steps, then push
#
#   ./do3.sh --rebase-only      step 2 only (rebase onto upstream/main)
#   ./do3.sh --pick-only        step 3 only (add the revision commit)
#
# The last two exist for the two-push flow: run --rebase-only, push, let CI run
# on the rebase alone, then run --pick-only and push again. The first of those
# pushes is forced (the base moved); the second is a plain fast-forward.
#
# Run it from any branch: it switches to the week-3 branch itself.

set -euo pipefail

# --- run from a copy outside the working tree ------------------------------
# This script is committed on fable-scaladoc-weeks-3-11 and does not exist on
# the week-3 branch, so the `git switch` below deletes it from the working
# tree. Bash reads a script incrementally, so that kills the run part-way
# through (verified: bash exits 2 on the next line it tries to read). Copying
# ourselves out of the repo and re-exec'ing makes the file we are reading
# immune to anything git does to the tree.
if [ -z "${DO3_DETACHED:-}" ]; then
  _self="$(cd "$(dirname "$0")" && pwd)/$(basename "$0")"
  _tmp="$(mktemp "${TMPDIR:-/tmp}/do3.XXXXXX")"
  cp "$_self" "$_tmp"
  DO3_DETACHED=1 exec bash "$_tmp" "$@"
fi
trap 'rm -f "$0"' EXIT     # $0 is the temp copy; clean it up on the way out

BRANCH=scaladoc-missing-docs-core-array-function-tuple-sys
OLD_BASE=ddd6514259                 # base the branch was cut from, 2026-07-30
REVISION=92a8136fbe                 # Week 3 revision
FIXUP=4199d44413                    # Week 3 audit fixup (wrapAccess doc placement)
SOURCE=fable-scaladoc-weeks-3-11    # branch the two commits come from (and where this script lives)
SELF_PATH=todo-writer/scripts/do3.sh
TARGET=upstream/main                # what to sit on top of
EXPECT_NEW_FILES=66                 # files in the commit being added
EXPECT_PR_FILES=109                 # files in the whole PR afterwards

DO_PUSH=no
STAGE=all
for arg in "$@"; do
  case "$arg" in
    --push)        DO_PUSH=yes ;;
    --rebase-only) STAGE=rebase ;;
    --pick-only)   STAGE=pick ;;
    *) printf 'unknown argument: %s\n' "$arg" >&2; exit 2 ;;
  esac
done

say()  { printf '\n=== %s\n' "$*"; }
die()  { printf '\nSTOP: %s\n' "$*" >&2; exit 1; }

# Return to the branch we started on. Worth doing because this script lives on
# $SOURCE and is not in the tree on $BRANCH, so staying on $BRANCH would leave
# you unable to invoke it again for the next stage. Only called on success;
# after a failure you are left where you are, to inspect.
go_home() {
  if [ -n "${START_BRANCH:-}" ] && [ "$(git branch --show-current || true)" != "$START_BRANCH" ]; then
    git switch -q "$START_BRANCH"
    echo
    echo "  (switched back to $START_BRANCH; $BRANCH keeps the work)"
  fi
}

# ---------------------------------------------------------------------------
# check <ref> [--base-only]
#
#   Three checks on every file that differs from <ref>:
#     1. it differs in comment lines only (no code touched)
#     2. no TODO FILL IN marker survives
#     3. no doc comment sits directly below an annotation, where the parser
#        drops it
#
#   With --base-only, only check 1 has to pass. That is right after the rebase
#   in step 2, because the branch's existing commit is the PRE-revision text:
#   it still has 17 unfilled markers (Function7.scala and AbstractFunction14
#   .scala, the two files cheeseng commented on) and several doc comments below
#   annotations. Fixing exactly those is what step 3 is for.
# ---------------------------------------------------------------------------
check() {
  local ref=${1:-$TARGET} mode=${2:-full}
  local n=0 code=0 marks=0 misplaced=0 f hits h
  for f in $(git diff --name-only "$ref" -- 'library/src' 'library-js/src'); do
    n=$((n+1))
    if [ -n "$(diff <(git show "$ref:$f" | grep -vE '^[[:space:]]*(\*|/\*\*|\*/)' | grep -v '^[[:space:]]*$') \
                    <(grep -vE '^[[:space:]]*(\*|/\*\*|\*/)' "$f" | grep -v '^[[:space:]]*$'))" ]; then
      echo "    NOT COMMENT-ONLY: $f"; code=$((code+1))
    fi
    local m
    m=$(grep -ac 'TODO FILL IN' "$f" || true)
    if [ "$m" != "0" ]; then
      if [ "$mode" = full ]; then echo "    LEFTOVER MARKER ($m): $f"; fi
      marks=$((marks+m))
    fi
    hits=$(awk '
      /^[[:space:]]*\/\*[^*]/ { inblk=1 }
      inblk { if (/\*\//) inblk=0; next }
      /^[[:space:]]*$/ { next }
      ann && /^[[:space:]]*\/\*\*/ { print FILENAME ":" NR }
      { ann = ($0 ~ /^[[:space:]]*@[A-Za-z_`]/ &&
               $0 !~ /(^|[^A-Za-z])(def|val|var|class|trait|object|type|given|enum)([^A-Za-z]|$)/) }
    ' "$f")
    if [ -n "$hits" ]; then
      for h in $hits; do
        if [ "$mode" = full ]; then echo "    DOC BELOW ANNOTATION: $h"; fi
        misplaced=$((misplaced+1))
      done
    fi
  done
  echo "    files: $n   code changes: $code   markers: $marks   docs below annotations: $misplaced"
  if [ "$mode" = base-only ]; then
    if [ "$marks" != 0 ] || [ "$misplaced" != 0 ]; then
      echo "    (expected here: this is the pre-revision text, and step 3 is what fixes it)"
    fi
    [ "$code" = 0 ]
  else
    [ "$code" = 0 ] && [ "$marks" = 0 ] && [ "$misplaced" = 0 ]
  fi
}

# ---------------------------------------------------------------------------
say "0. preflight"
# ---------------------------------------------------------------------------
cd "$(git rev-parse --show-toplevel)"
[ -z "$(git status --porcelain --untracked-files=no)" ] || die "working tree is dirty; commit or stash first"
START_BRANCH=$(git branch --show-current || true)

git fetch -q upstream
git fetch -q origin
echo "  upstream/main = $(git rev-parse --short upstream/main)  ($(git log -1 --format=%ad --date=short upstream/main))"
echo "  local main    = $(git rev-parse --short main)"
behind=$(git rev-list --count main..upstream/main)
if [ "$behind" != 0 ]; then echo "  note: your local main is $behind commit(s) behind upstream/main."; fi
echo "        This script targets upstream/main directly, so that does not matter here."

git rev-parse --verify -q "$SOURCE" >/dev/null || die "$SOURCE not found; the two commits come from it"
for c in "$REVISION" "$FIXUP"; do
  git cat-file -e "$c" 2>/dev/null || die "commit $c not found"
done

git rev-parse --verify -q "$BRANCH" >/dev/null || die "local branch $BRANCH not found"
origin_sha=$(git rev-parse "origin/$BRANCH")

if [ "$STAGE" = pick ]; then
  # --pick-only: the rebase has already happened, so expect exactly one commit
  # sitting on top of TARGET.
  [ "$(git merge-base "$BRANCH" "$TARGET")" = "$(git rev-parse "$TARGET")" ] || \
    die "$BRANCH is not on top of $TARGET; run without --pick-only, or rebase first"
  n_commits=$(git rev-list --count "$TARGET..$BRANCH")
  echo "  $BRANCH = $(git rev-parse --short "$BRANCH"), $n_commits commit(s) on $TARGET"
  [ "$n_commits" = 1 ] || die "expected 1 commit on top of $TARGET, found $n_commits"
else
  # The branch must be exactly what origin has, and one commit on OLD_BASE.
  [ "$(git rev-parse "$BRANCH")" = "$origin_sha" ] || \
    die "local $BRANCH ($(git rev-parse --short "$BRANCH")) != origin/$BRANCH ($(git rev-parse --short "origin/$BRANCH")); reconcile them first"
  [ "$(git merge-base "$BRANCH" "$TARGET")" = "$(git rev-parse "$OLD_BASE")" ] || \
    die "$BRANCH is not sitting on $OLD_BASE any more; re-check before running this"
  n_commits=$(git rev-list --count "$OLD_BASE..$BRANCH")
  echo "  $BRANCH = $(git rev-parse --short "$BRANCH"), $n_commits commit(s) on $OLD_BASE"
  [ "$n_commits" = 1 ] || die "expected 1 commit on the branch, found $n_commits"
fi

# ---------------------------------------------------------------------------
say "1. safety net"
# ---------------------------------------------------------------------------
BACKUP="backup/${BRANCH}-$(date +%Y%m%d-%H%M%S)"
git branch "$BACKUP" "$BRANCH"
echo "  branch $BACKUP now points at the pre-change state"
echo "  to undo everything this script does:  git switch $BRANCH && git reset --hard $BACKUP"

# ---------------------------------------------------------------------------
git switch -q "$BRANCH"

if [ "$STAGE" = pick ]; then
  say "2. rebase onto $TARGET - SKIPPED (--pick-only)"
else
say "2. rebase the existing commit onto $TARGET"
# ---------------------------------------------------------------------------
if ! git rebase --onto "$TARGET" "$OLD_BASE" "$BRANCH"; then
  echo
  echo "  The rebase stopped. Unresolved paths:"
  git diff --name-only --diff-filter=U | sed 's/^/    /'
  echo
  echo "  Resolve, 'git add' them, then 'git rebase --continue'; or 'git rebase --abort'"
  echo "  and then 'git reset --hard $BACKUP' to get back to where you started."
  echo "  (This was clean in my dry run, so something has moved since.)"
  exit 1
fi
echo "  now $(git rev-list --count "$TARGET"..HEAD) commit on $TARGET"

say "   the rebased commit, checked against $TARGET"
echo "  (this is still the PRE-revision text, so markers and misplaced docs are"
echo "   expected here; step 3 is what removes them)"
check "$TARGET" base-only || die "the rebased commit touches code, not just comments"

if [ "$STAGE" = rebase ]; then
  say "stopping after the rebase (--rebase-only)"
  if [ "$DO_PUSH" = yes ]; then
    git push --force-with-lease="$BRANCH:$origin_sha" origin "$BRANCH"
    echo "  pushed. Let CI run, then: bash $SELF_PATH --pick-only"
  else
    echo "  Push it with:"
    echo "     git push --force-with-lease=$BRANCH:$origin_sha origin $BRANCH"
    echo "  then, once you are happy:  bash $SELF_PATH --pick-only"
  fi
  go_home
  echo
  echo "Backup of the original branch: $BACKUP"
  exit 0
fi
fi

# ---------------------------------------------------------------------------
say "3. cherry-pick the revision and the audit fixup as one commit"
# ---------------------------------------------------------------------------
# $REVISION rewrites the documentation added by the branch's first commit,
# checking every claim against the implementation. $FIXUP moves
# SystemProperties.wrapAccess's doc above its @nowarn annotation, where a doc
# comment below an annotation is dropped by the parser.
if ! git cherry-pick -n "$REVISION" "$FIXUP"; then
  echo
  echo "  The cherry-pick stopped. Unresolved paths:"
  git diff --name-only --diff-filter=U | sed 's/^/    /'
  echo
  echo "  Resolve and 'git add' them, then finish with:"
  echo "     git cherry-pick --quit && git commit"
  echo "  Or back out entirely:  git cherry-pick --quit && git reset --hard $BACKUP"
  exit 1
fi

staged=$(git diff --cached --name-only | wc -l | tr -d ' ')
echo "  $staged file(s) staged (expected $EXPECT_NEW_FILES)"
[ "$staged" = "$EXPECT_NEW_FILES" ] || echo "  note: that is not the expected count; look before you commit"
git diff --cached --name-only | grep -v '^library/' && die "something outside library/ is staged" || true

git commit -q -F - <<'MSG'
Revise the added Scaladoc for the core root files, Function/Tuple/Product and sys

Rewrites the documentation added by the first commit on this branch, checking
every claim against the implementation rather than against the name: methods
whose body is `this`, parameters that are never used, exceptions that cannot
actually be thrown, and `@param` tags naming the wrong parameter.

Also moves SystemProperties.wrapAccess's doc comment above its `@nowarn`
annotation. A doc comment below an annotation is dropped by the parser, so
that entry was rendering undocumented.

Still comment-only: no declaration, body, import, annotation or blank line
is touched.
MSG
echo "  committed: $(git log -1 --format='%h %s')"

# ---------------------------------------------------------------------------
say "4. verify"
# ---------------------------------------------------------------------------
echo "  the commit just added:"
check HEAD~1 || die "the added commit did not come out clean"
echo "  the whole PR against $TARGET:"
check "$TARGET" || die "the branch did not come out clean against $TARGET"

say "5. cross-check against $SOURCE"
same=0; dif=0
for f in $(git diff --name-only "$TARGET"..HEAD -- 'library/src' 'library-js/src'); do
  if git show "HEAD:$f" | diff -q - <(git show "$SOURCE:$f") >/dev/null 2>&1; then
    same=$((same+1))
  else
    dif=$((dif+1)); echo "  differs from $SOURCE: $f"
  fi
done
echo "  identical to $SOURCE: $same   differing: $dif"
[ "$dif" = 0 ] || echo "  (differences here are upstream code drift, not documentation; inspect the list)"

total=$(git diff --name-only "$TARGET"..HEAD | wc -l | tr -d ' ')
echo
echo "  result: $(git rev-list --count "$TARGET"..HEAD) commits on $TARGET, $total files (expected 2 and $EXPECT_PR_FILES)"

# ---------------------------------------------------------------------------
say "6. push"
# ---------------------------------------------------------------------------
# The base moved, so this push rewrites what origin has: it must be forced.
# --force-with-lease refuses if origin/$BRANCH is not still $origin_sha, which
# is the protection you want on a live draft PR.
if [ "$DO_PUSH" = yes ]; then
  git push --force-with-lease="$BRANCH:$origin_sha" origin "$BRANCH"
  echo "  pushed."
else
  echo "  Not pushing (run with --push, or do it yourself):"
  echo
  echo "     git push --force-with-lease=$BRANCH:$origin_sha origin $BRANCH"
  echo
  echo "  The lease is against $(git rev-parse --short "$origin_sha"), what origin held when this"
  echo "  script started, so the push refuses if anything else moved the branch."
fi

go_home
echo
echo "Done. Backup of the original branch: $BACKUP"
echo "Undo everything:  git switch $BRANCH && git reset --hard $BACKUP"
