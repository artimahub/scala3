#!/usr/bin/env python3
"""Split non-user-facing Scaladoc off the documentation PR branches.

A declaration is *user-facing* when the published Scaladoc renders it: it is
public, or plain `protected`. Everything else - `private`, `private[x]`,
`protected[this]`, `protected[x]`, and anything nested inside such a scope -
is hidden, because scaladoc's `isHiddenByVisibility`
(scaladoc/src/dotty/tools/scaladoc/tasty/SymOps.scala:126) drops it unless
`-private` is passed, and no build here passes it.

Subcommands
    report   classify every added doc line; write JSON + Markdown. Read-only.
    split    rewrite each PR branch to carry only the user-facing docs, and
             park the rest on a companion branch. Dry-run unless --apply.

The split is content-preserving by construction:

    tree(<branch>-internal-docs) == tree(<branch> before the split)

so nothing is lost; the internal docs simply move one commit downstream and
can be merged, deferred, or dropped on their own.

No working tree is touched: trees are assembled with a temporary index and
committed with `git commit-tree` (this repo's sandbox has a stat-cache race
that makes checkout/rebase unreliable).
"""

import argparse, collections, json, os, re, subprocess, sys, tempfile

REPO = os.environ.get("SCALA3_REPO", "/workspace/scala3")

# The 12 branches of the latest batch. `scaladoc-missing-docs-io-ref-numeric`
# (week 1) is deliberately absent: it is the older, separately opened PR.
BRANCHES = [
    "scaladoc-missing-docs-annotation-reflect-root",
    "scaladoc-missing-docs-misc-dirs-root-misc",
    "scaladoc-missing-docs-core-array-function-tuple-sys",
    "scaladoc-missing-docs-util-concurrent",
    "scaladoc-missing-docs-math-coll-generic",
    "scaladoc-missing-docs-quoted-jdk",
    "scaladoc-missing-docs-collection-convert-js",
    "scaladoc-missing-docs-runtime",
    "scaladoc-missing-docs-collection-mutable",
    "scaladoc-missing-docs-collection-core",
    "scaladoc-missing-docs-collection-immutable-vector-hashmap-arrayseq",
    "scaladoc-missing-docs-collection-immutable-list-lazylist-sorted",
]

SUFFIX = "-internal-docs"
UPSTREAM = "upstream/main"

DATA = "todo-writer/reviews/internal-docs.json"
REPORT = "todo-writer/docs/internal-docs-inventory.md"


# ---------------------------------------------------------------- git helpers

def git(*args, check=True):
    p = subprocess.run(("git",) + args, cwd=REPO, capture_output=True, text=True)
    if check and p.returncode != 0:
        raise SystemExit("git %s failed:\n%s" % (" ".join(args), p.stderr))
    return p.stdout


def show(rev, path):
    return git("show", "%s:%s" % (rev, path)).split("\n")


# ------------------------------------------------------------------- parsing

MODS = (r"(?:final|sealed|abstract|implicit|lazy|override|case|inline|transparent"
        r"|opaque|open|infix|erased|@\w+(?:\([^)]*\))?)")
DECL = re.compile(
    r"^(?P<ind>\s*)(?P<mods>(?:(?:private|protected)(?:\[[^\]]*\])?|" + MODS + r")\s+)*"
    r"(?P<kw>def|val|var|type|class|trait|object|enum|given|extension|package)\b"
    r"\s*(?P<name>[A-Za-z_`][\w`$]*)?")
VIS = re.compile(r"\b(private|protected)(\[([^\]]*)\])?")
DOCLINE = re.compile(r"^\s*(/\*\*|\*|\*/)")
ANNO = re.compile(r"^\s*@")
CONTAINERS = ("class", "trait", "object", "enum", "package")


def visibility(line):
    m = DECL.match(line)
    if not m:
        return None
    v = VIS.search(line[:m.start("kw")])
    if not v:
        return "public"
    if v.group(1) == "protected":
        return "protected" if v.group(3) is None else "protected[%s]" % v.group(3)
    if v.group(3) is None:
        return "private"
    return "private[%s]" % v.group(3)


def strictness(v):
    """Higher is more restrictive. Only `public` and plain `protected` render."""
    if v == "public":
        return 0
    if v == "protected":
        return 1
    if v.startswith("protected["):
        return 2
    if v == "private[this]" or v == "private":
        return 4
    return 3  # private[x]


def renders(v):
    return v in ("public", "protected")


def is_skippable(l):
    """Blank, comment, or a bare annotation - i.e. not the declaration itself.

    `@inline def foo` and `@SerialVersionUID(3L) class Bar` are declarations,
    so DECL is tried before the annotation test."""
    if DECL.match(l):
        return False
    return (not l.strip()) or DOCLINE.match(l) or ANNO.match(l) or l.lstrip().startswith("//")


def containers_of(lines):
    out = []
    for i, l in enumerate(lines):
        if is_skippable(l):
            continue
        m = DECL.match(l)
        if m and m.group("kw") in CONTAINERS:
            out.append((i, len(m.group("ind")), m.group("kw"), m.group("name") or "?", visibility(l)))
    return out


def enclosing(conts, lineno, indent):
    res, need = [], indent
    for (ln, ind, kw, nm, v) in reversed(conts):
        if ln < lineno and ind < need:
            res.append((kw, nm, v))
            need = ind
            if need == 0:
                break
    return list(reversed(res))


def added_lines(base, tip, path):
    """1-based line numbers in `tip`'s version of `path` that the branch added."""
    diff = git("diff", "-U0", base, tip, "--", path)
    added, n = set(), 0
    for l in diff.split("\n"):
        if l.startswith("@@"):
            m = re.search(r"\+(\d+)", l)
            n = int(m.group(1))
            continue
        if l.startswith("+++") or l.startswith("---"):
            continue
        if l.startswith("+"):
            added.add(n)
            n += 1
        elif not l.startswith("-"):
            n += 1
    return added


def removed_lines(base, tip, path):
    """1-based line numbers in `base`'s version of `path` that the branch removed."""
    diff = git("diff", "-U0", base, tip, "--", path)
    gone, n = set(), 0
    for l in diff.split("\n"):
        if l.startswith("@@"):
            m = re.search(r"@@ -(\d+)", l)
            n = int(m.group(1))
            continue
        if l.startswith("+++") or l.startswith("---"):
            continue
        if l.startswith("-"):
            gone.add(n)
            n += 1
        elif not l.startswith("+"):
            n += 1
    return gone


def comment_depth_ok(lines):
    """True if `/* ... */` nesting never goes negative and closes out.

    Counts `/*` and `*/` as tokens, so a one-line `/** x */` and the file's
    `/* ... */` licence header both balance. Approximate - a `*/` inside a
    string literal would fool it - which is why callers compare the result
    against the same measurement on the base file rather than trusting it
    outright."""
    depth = 0
    for l in lines:
        i = 0
        while i < len(l) - 1:
            two = l[i:i + 2]
            if two == "/*":
                depth += 1
                i += 2
            elif two == "*/":
                depth -= 1
                if depth < 0:
                    return False
                i += 2
            else:
                i += 1
    return depth == 0


# ---------------------------------------------------------------- classifying

def classify_file(branch, base, path):
    """-> (records, internal_line_numbers, unattributed_line_numbers)"""
    src = show(branch, path)
    added = added_lines(base, branch, path)
    if not added:
        return [], set(), set()
    conts = containers_of(src)
    doc_added = sorted(i for i in added if i - 1 < len(src) and DOCLINE.match(src[i - 1]))

    records, internal, attributed = [], set(), set()
    by_decl = collections.defaultdict(list)
    for i in doc_added:
        j = i - 1
        while j < len(src) and is_skippable(src[j]):
            j += 1
        if j >= len(src) or not DECL.match(src[j]):
            continue
        by_decl[j].append(i)

    for j, lines in sorted(by_decl.items()):
        m = DECL.match(src[j])
        own = visibility(src[j])
        encl = enclosing(conts, j, len(m.group("ind")))
        eff, why = own, None
        for (kw, nm, v) in encl:
            if strictness(v) > strictness(eff):
                eff, why = v, "%s %s" % (kw, nm)
        rec = dict(branch=branch, file=path, decl_line=j + 1, kw=m.group("kw"),
                   name=m.group("name") or "?", own=own, effective=eff,
                   hidden=not renders(eff), hidden_by=why,
                   enclosing=[{"kw": k, "name": n, "vis": v} for k, n, v in encl],
                   doc_lines=sorted(lines))
        records.append(rec)
        attributed.update(lines)
        if rec["hidden"]:
            internal.update(lines)

    return records, internal, sorted(set(doc_added) - attributed)


def collect():
    out, unattributed, deletions = [], [], []
    for b in BRANCHES:
        base = git("merge-base", UPSTREAM, b).strip()
        files = [f for f in git("diff", "--name-only", base, b).split() if f.endswith(".scala")]
        for f in files:
            recs, _internal, un = classify_file(b, base, f)
            out.extend(recs)
            if un:
                unattributed.append(dict(branch=b, file=f, lines=un))
            rm = removed_lines(base, b, f)
            if rm:
                deletions.append(dict(branch=b, file=f, removed=len(rm)))
    return out, unattributed, deletions


# ------------------------------------------------------------------ reporting

def write_report(recs, unattributed, deletions):
    total = len(recs)
    hid = [r for r in recs if r["hidden"]]
    by_eff = collections.Counter(r["effective"] for r in hid)
    lines = []
    A = lines.append

    A("# Non-user-facing Scaladoc in the 12 documentation PRs")
    A("")
    A("Generated by `todo-writer/scripts/split-internal-docs.py report`.")
    A("")
    A("A declaration is **user-facing** when the published Scaladoc renders it:")
    A("public, or plain `protected`. Everything else is hidden by scaladoc's")
    A("`isHiddenByVisibility` (`scaladoc/src/dotty/tools/scaladoc/tasty/SymOps.scala:126`),")
    A("which drops every `Visibility.Private(_)` - qualified or not - plus")
    A("`protected[this]` and `protected[x]`, unless `-private` is passed.")
    A("`includePrivateAPI` defaults to false and no build in this repo sets it.")
    A("A member nested in a hidden container is itself unreachable, so it counts")
    A("as hidden even when it carries no modifier of its own.")
    A("")
    A("## Totals")
    A("")
    A("| | count | share |")
    A("|---|---:|---:|")
    A("| declarations given documentation | %d | 100%% |" % total)
    A("| **hidden from the published docs** | **%d** | **%.1f%%** |" % (len(hid), 100.0 * len(hid) / total))
    A("| user-facing | %d | %.1f%% |" % (total - len(hid), 100.0 * (total - len(hid)) / total))
    A("")
    A("How the hidden ones became hidden:")
    A("")
    A("| cause | count |")
    A("|---|---:|")
    own_pub = sum(1 for r in hid if r["own"] == "public")
    A("| public itself; hidden only by an enclosing private scope | %d |" % own_pub)
    A("| carries its own restrictive modifier | %d |" % (len(hid) - own_pub))
    A("")
    A("Effective visibility of the hidden set:")
    A("")
    A("| visibility | count |")
    A("|---|---:|")
    for v, n in by_eff.most_common():
        A("| `%s` | %d |" % (v, n))
    A("")

    A("## By branch")
    A("")
    A("| branch | documented | hidden | share |")
    A("|---|---:|---:|---:|")
    for b in BRANCHES:
        d = [r for r in recs if r["branch"] == b]
        h = [r for r in d if r["hidden"]]
        if not d:
            continue
        A("| `%s` | %d | %d | %.1f%% |" % (b, len(d), len(h), 100.0 * len(h) / len(d)))
    A("")

    A("## Wholly internal files")
    A("")
    A("Every documented declaration in these files is hidden, so removing their")
    A("documentation costs no user-facing content at all.")
    A("")
    tot = collections.Counter(r["file"] for r in recs)
    hct = collections.Counter(r["file"] for r in hid)
    whole = sorted([(f, tot[f]) for f in tot if hct[f] == tot[f]], key=lambda x: -x[1])
    A("%d files, %d declarations." % (len(whole), sum(n for _, n in whole)))
    A("")
    A("| declarations | file |")
    A("|---:|---|")
    for f, n in whole:
        A("| %d | `%s` |" % (n, f))
    A("")

    A("## Mixed files")
    A("")
    A("These keep user-facing documentation; only the listed share moves out.")
    A("")
    A("| hidden | of | file |")
    A("|---:|---:|---|")
    for f, n in sorted(hct.items(), key=lambda x: -x[1]):
        if hct[f] != tot[f]:
            A("| %d | %d | `%s` |" % (n, tot[f], f))
    A("")

    A("## Caveats")
    A("")
    A("The classifier is a regex plus an indentation-based scope walker, not a")
    A("compiler front end. Expect a small error rate on unusual layouts. The")
    A("`split` subcommand does not rely on the classification being perfect: it")
    A("verifies that the two resulting trees recombine to exactly the current")
    A("tree, that each rewritten branch is still comment-only against its merge")
    A("base, and that no `TODO FILL IN` marker reappears.")
    A("")
    if unattributed:
        A("Added doc lines that could not be attributed to a declaration "
          "(left on the user-facing branch):")
        A("")
        for u in unattributed:
            A("- `%s` %s: lines %s" % (u["branch"], u["file"],
                                       ", ".join(str(x) for x in u["lines"][:12])))
        A("")
    else:
        A("Every added doc line was attributed to a declaration.")
        A("")
    if deletions:
        A("Files where the branch also *removed* lines present in its merge base "
          "(these stay on the user-facing branch and are not split):")
        A("")
        for d in deletions:
            A("- `%s` %s: %d line(s)" % (d["branch"], d["file"], d["removed"]))
        A("")

    with open(os.path.join(REPO, REPORT), "w", encoding="utf-8") as fh:
        fh.write("\n".join(lines))
    return len(hid), total


# ---------------------------------------------------------------- the split

def build_tree(rev, edits):
    """Copy tree of `rev`, replacing the files in `edits` ({path: [lines]})."""
    idx = tempfile.mktemp(prefix="split-idx-")
    env = dict(os.environ, GIT_INDEX_FILE=idx)
    try:
        subprocess.run(["git", "read-tree", rev], cwd=REPO, env=env, check=True,
                       capture_output=True)
        for path, lines in edits.items():
            blob = subprocess.run(["git", "hash-object", "-w", "--stdin"], cwd=REPO,
                                  input="\n".join(lines), capture_output=True,
                                  text=True, check=True).stdout.strip()
            mode = git("ls-tree", rev, "--", path).split()[0]
            subprocess.run(["git", "update-index", "--index-info"], cwd=REPO, env=env,
                           input="%s %s\t%s\n" % (mode, blob, path), text=True,
                           check=True, capture_output=True)
        return subprocess.run(["git", "write-tree"], cwd=REPO, env=env,
                              capture_output=True, text=True, check=True).stdout.strip()
    finally:
        if os.path.exists(idx):
            os.unlink(idx)


def noncomment(lines):
    out = []
    for l in lines:
        if DOCLINE.match(l) or not l.strip():
            continue
        out.append(l)
    return out


def split(apply, only=None, prefix=""):
    recs = json.load(open(os.path.join(REPO, DATA), encoding="utf-8"))["records"]
    by_branch = collections.defaultdict(list)
    for r in recs:
        if r["hidden"]:
            by_branch[r["branch"]].append(r)

    plan = []
    for b in BRANCHES:
        if only and b != only:
            continue
        tip = git("rev-parse", b).strip()
        base = git("merge-base", UPSTREAM, b).strip()
        drop = collections.defaultdict(set)
        for r in by_branch[b]:
            drop[r["file"]].update(r["doc_lines"])
        if not drop:
            print("  %s: nothing hidden, leaving alone" % b)
            continue

        # A file where the branch also *deleted* base lines is not safe to
        # thin automatically: reverting a hidden declaration's added lines
        # would leave the pre-existing text it replaced deleted as well. Such
        # files keep all their documentation on the user-facing branch.
        edits, moved, skipped = {}, 0, []
        for path, lines in sorted(drop.items()):
            if removed_lines(base, tip, path):
                skipped.append((path, len(lines)))
                continue
            src = show(tip, path)
            edits[path] = [l for i, l in enumerate(src, 1) if i not in lines]
            moved += len(lines)

        if not edits:
            print("  %s: every affected file rewrites base text, skipping" % b)
            continue

        pub_tree = build_tree(tip, edits)

        # --- verification -------------------------------------------------
        problems = []
        for path in edits:
            b_src, p_src = show(base, path), edits[path]
            if noncomment(b_src) != noncomment(p_src):
                problems.append("not comment-only vs base: %s" % path)
            if any("TODO FILL IN" in l for l in p_src):
                problems.append("marker reappeared: %s" % path)
            if comment_depth_ok(b_src) and not comment_depth_ok(p_src):
                problems.append("comment nesting broken: %s" % path)
        if problems:
            print("  %s: ABORT" % b)
            for p in problems:
                print("      %s" % p)
            continue

        plan.append(dict(branch=b, tip=tip, pub_tree=pub_tree,
                         files=len(edits), lines=moved,
                         decls=len(by_branch[b]), skipped=skipped))
        note = ("  (%d file(s) held back, %d doc lines)"
                % (len(skipped), sum(n for _, n in skipped))) if skipped else ""
        print("  %-64s %4d decls, %4d doc lines, %2d files%s" %
              (b, len(by_branch[b]), moved, len(edits), note))

    if not apply:
        print("\nDry run. Re-run with --apply to write the branches.")
        return

    print()
    for p in plan:
        b, tip = p["branch"], p["tip"]
        msg_pub = ("Move non-user-facing Scaladoc to %s%s\n\n"
                   "%d declarations in %d file(s) are hidden from the published\n"
                   "Scaladoc: private, private[x], protected[x], or nested inside\n"
                   "such a scope. Their documentation moves to the companion branch\n"
                   "so this PR carries only what the API pages render.\n\n"
                   "No content is lost: the companion branch's tree is identical to\n"
                   "this branch's tree before the split.\n" % (b, SUFFIX, p["decls"], p["files"]))
        pub = git("commit-tree", p["pub_tree"], "-p", tip, "-F", "-",
                  check=True) if False else subprocess.run(
            ["git", "commit-tree", p["pub_tree"], "-p", tip], cwd=REPO,
            input=msg_pub, capture_output=True, text=True, check=True).stdout.strip()

        msg_int = ("Scaladoc for non-user-facing declarations in %s\n\n"
                   "The %d declarations split out of %s: private, private[x],\n"
                   "protected[x], or nested inside such a scope, so the published\n"
                   "Scaladoc does not render them.\n\n"
                   "This restores the tree exactly as it stood before the split.\n"
                   % (b, p["decls"], b))
        tip_tree = git("rev-parse", "%s^{tree}" % tip).strip()
        internal = subprocess.run(
            ["git", "commit-tree", tip_tree, "-p", pub], cwd=REPO,
            input=msg_int, capture_output=True, text=True, check=True).stdout.strip()

        # the companion branch must reproduce the original tree, exactly
        if git("diff", "--stat", tip, internal).strip():
            raise SystemExit("REFUSING: %s%s does not reproduce %s" % (b, SUFFIX, b))

        pub_ref, int_ref = prefix + b, prefix + b + SUFFIX
        git("branch", "-f", pub_ref, pub)
        git("branch", "-f", int_ref, internal)
        print("  %s -> %s   %s -> %s" % (pub_ref, pub[:10], int_ref, internal[:10]))

    if prefix:
        print("\nRehearsal written under '%s'. The real branches are untouched." % prefix)
        return
    print("\nBranches rewritten locally. Nothing pushed.")
    print("Review, then push each pair:")
    print("  git push --force-with-lease origin <branch>")
    print("  git push origin <branch>%s" % SUFFIX)


# ------------------------------------------------------------------------ cli

def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = ap.add_subparsers(dest="cmd", required=True)
    sub.add_parser("report", help="classify and write the inventory (read-only)")
    sp = sub.add_parser("split", help="split the branches (dry-run unless --apply)")
    sp.add_argument("--apply", action="store_true")
    sp.add_argument("--only", help="operate on a single branch")
    sp.add_argument("--prefix", default="",
                    help="write to <prefix><branch> instead of rewriting in place; "
                         "use for a rehearsal, e.g. --prefix trial/")
    a = ap.parse_args()

    if a.cmd == "report":
        recs, un, dels = collect()
        os.makedirs(os.path.dirname(os.path.join(REPO, DATA)), exist_ok=True)
        with open(os.path.join(REPO, DATA), "w", encoding="utf-8") as fh:
            json.dump(dict(branches=BRANCHES, records=recs,
                           unattributed=un, deletions=dels), fh, indent=1)
        hid, total = write_report(recs, un, dels)
        print("%d of %d documented declarations are hidden (%.1f%%)"
              % (hid, total, 100.0 * hid / total))
        print("wrote %s and %s" % (DATA, REPORT))
    else:
        split(a.apply, a.only, a.prefix)


if __name__ == "__main__":
    main()
