#!/usr/bin/env python3
"""Group the doc comments a writer pass generated more than once.

Why this exists
---------------
Week 4's DurationConversions.scala drew 20 review comments that were all the
same comment. The writer had produced one wrong `@param` line:

    @param c the classifier instance      (wrong: `ev` is the classifier, not `c`)

and copied it into 20 near-identical `days`/`hours`/`minutes` methods. One
mistake, replicated; the reviewers judged each copy independently, the
adjudicator settled it 46 times, and the human then typed the same sentence out
20 times on the PR.

Repetition of that shape is normal in the stdlib: `Ordering`, `Numeric`,
`BigDecimal` and `DurationConversions` are all built from families of members
that differ only in a type or a unit. So instead of pretending each copy is a
fresh judgement, this finds the identical blocks and hands the reviewer a group:
judge it once, and the ruling applies to every occurrence.

Two blocks are "identical" after whitespace normalisation, nothing cleverer.
A near-miss (two blocks differing in one word) is deliberately NOT grouped: the
difference might be the whole point, and a wrong merge would hide a real error.

Usage:
    repeated-doc-blocks.py --orig OLD --new NEW [--min 3]

Writes a review-prompt section to stdout, or nothing at all when no block
repeats. Silence is the common case and means "nothing to group".
"""

import argparse
import difflib
import re
import sys
from collections import defaultdict


def added_line_runs(orig_lines, new_lines):
    """Contiguous runs of lines present in NEW and not in OLD.

    Yields (start_line_number_1_based, [lines]).
    """
    matcher = difflib.SequenceMatcher(None, orig_lines, new_lines, autojunk=False)
    for tag, _i1, _i2, j1, j2 in matcher.get_opcodes():
        if tag in ("insert", "replace") and j2 > j1:
            yield j1 + 1, new_lines[j1:j2]


def doc_blocks(start, lines):
    """Split a run of added lines into complete Scaladoc comments.

    A block runs from a line containing `/**` to the line containing `*/`.
    One-line comments (`/** ... */`) count. Anything outside a comment, and any
    comment left unterminated at the end of the run, is dropped: an incomplete
    block cannot be compared safely against a complete one.
    """
    block, block_start = [], None
    for offset, line in enumerate(lines):
        stripped = line.strip()
        if block_start is None:
            if stripped.startswith("/**"):
                block_start = start + offset
                block = [line]
                if "*/" in stripped[3:]:
                    yield block_start, block
                    block, block_start = [], None
        else:
            block.append(line)
            if "*/" in stripped:
                yield block_start, block
                block, block_start = [], None


def normalise(block):
    """Collapse whitespace and comment furniture so formatting is not a difference."""
    out = []
    for line in block:
        text = line.strip()
        text = re.sub(r"^/\*\*", "", text)
        text = re.sub(r"\*/$", "", text)
        text = re.sub(r"^\*+", "", text)
        text = re.sub(r"\s+", " ", text).strip()
        if text:
            out.append(text)
    return "\n".join(out)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--orig", required=True)
    ap.add_argument("--new", required=True)
    ap.add_argument("--min", type=int, default=3,
                    help="occurrences before a block is reported as a group")
    args = ap.parse_args()

    with open(args.orig, encoding="utf-8", errors="replace") as f:
        orig_lines = f.read().splitlines()
    with open(args.new, encoding="utf-8", errors="replace") as f:
        new_lines = f.read().splitlines()

    groups = defaultdict(list)   # normalised text -> [(line, raw_block)]
    for start, run in added_line_runs(orig_lines, new_lines):
        for block_start, block in doc_blocks(start, run):
            key = normalise(block)
            if key:
                groups[key].append((block_start, block))

    # Tag lines are grouped separately, and this is the part that would have
    # caught week 4. DurationConversions' blocks were NOT identical to each
    # other (one said nanoseconds, the next microseconds), so whole-block
    # grouping collapsed 20 copies into 5 groups. The wrong line itself,
    # `@param c the classifier instance`, was identical in all 20.
    tag_lines = defaultdict(list)
    for start, run in added_line_runs(orig_lines, new_lines):
        for offset, line in enumerate(run):
            text = re.sub(r"^\*+", "", line.strip().lstrip("/")).strip()
            if text.startswith("@"):
                tag_lines[re.sub(r"\s+", " ", text)].append(start + offset)

    repeated = [(k, v) for k, v in groups.items() if len(v) >= args.min]
    repeated_tags = [(k, v) for k, v in tag_lines.items() if len(v) >= args.min]
    if not repeated and not repeated_tags:
        return 0
    repeated.sort(key=lambda kv: -len(kv[1]))
    repeated_tags.sort(key=lambda kv: -len(kv[1]))

    print()
    print("=== REPEATED DOC BLOCKS (the writer generated these identically, more than once) ===")
    print()
    print("Each group below is ONE piece of writing that was applied to several")
    print("declarations. Judge each group ONCE, against the declaration at its first")
    print("occurrence, and raise at most one item per group.")
    print()
    print("When you raise an item against a group, say so in the issue text:")
    print('  "applies to all N occurrences of this block (lines ...)"')
    print("The refine step will apply your ruling to every occurrence, so do not file")
    print("the same finding N times -- that is exactly what this section exists to stop.")
    print()
    print("A caveat worth your attention: a block being repeated is itself evidence.")
    print("Prose that is true of the first declaration can be false of the fifth, since")
    print("only the declarations differ. Check the group text against EVERY member it")
    print("was applied to, not just the first, and if it does not fit them all, say")
    print("which ones it does not fit.")
    print()
    for n, (_key, occurrences) in enumerate(repeated, start=1):
        lines_at = ", ".join(str(line) for line, _ in occurrences)
        print(f"--- group {n}: {len(occurrences)} occurrences at new-file lines {lines_at} ---")
        for raw in occurrences[0][1]:
            print(raw)
        print()

    if repeated_tags:
        print("=== REPEATED TAG LINES (same tag text, many declarations) ===")
        print()
        print("These individual tag lines were generated identically across several")
        print("declarations. A tag can be right for one member and wrong for the rest,")
        print("and one wrong tag replicated is one item, not N. Check each against every")
        print("declaration listed, and raise it once with the full occurrence count.")
        print()
        for text, lines_at in repeated_tags:
            shown = ", ".join(str(n) for n in lines_at[:12])
            more = "" if len(lines_at) <= 12 else f", ... (+{len(lines_at) - 12} more)"
            print(f"  {len(lines_at):3d}x  {text}")
            print(f"        at lines {shown}{more}")
        print()
    return 0


if __name__ == "__main__":
    sys.exit(main())
