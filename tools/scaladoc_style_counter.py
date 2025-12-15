#!/usr/bin/env python3

from __future__ import annotations
import os
import re
import argparse
from typing import List, Tuple, Optional, Dict
from collections import Counter

SCALAR_EXTS = (".scala",)

BLOCK_RE = re.compile(r"/\*\*(?:.|\n)*?\*/", re.DOTALL)

def classify_alignment(block: str, opening_star_idxs: Tuple[int, Optional[int]], opening_line: str) -> Optional[str]:
    """
    Classify a multi-line scaladoc block alignment relative to the opening "/**".
    Returns: 'first', 'second', or 'others' (None if unknown).

    Rules (strict):
    - If the opening line contains text immediately after "/**", it's 'first'.
    - Expected columns for 'first' and 'second' are the positions of the first and
      second '*' characters in the opening line. No tolerance is applied.
    - If any interior '*' line has an indent exactly equal to the first '*' column,
      classify as 'first'. If any equals the second '*' column, classify as 'second'.
    - Otherwise fall back to the most-common interior indent and compare exactly.
    """
    first_idx, second_idx = opening_star_idxs

    # inline-text after "/**" -> 'first'
    if opening_line:
        pos = opening_line.find("/**")
        if pos != -1:
            rest = opening_line[pos + 3 :]
            if rest.strip() != "" and not rest.lstrip().startswith("*"):
                return "first"

    # prepare interior lines
    lines = block.splitlines()
    if lines and lines[0].lstrip().startswith("/**"):
        lines = lines[1:]
    if lines and lines[-1].rstrip().endswith("*/"):
        lines = lines[:-1]

    indents = []
    for ln in lines:
        if ln.strip() == "":
            continue
        m = re.match(r"^(?P<indent>\s*)\*(?:\s|$).*", ln)
        if m:
            indents.append(len(m.group("indent")))
    if not indents:
        return None

    # expected columns are the star positions (as returned by find in opening line)
    first_expected = first_idx if (first_idx is not None and first_idx != -1) else None
    second_expected = second_idx if (second_idx is not None and second_idx != -1) else None

    # prefer any exact interior-line match
    if first_expected is not None and any(i == first_expected for i in indents):
        return "first"
    if second_expected is not None and any(i == second_expected for i in indents):
        return "second"

    # fall back to most common indent (still require exact match)
    most_common_indent = Counter(indents).most_common(1)[0][0]
    if first_expected is not None and most_common_indent == first_expected:
        return "first"
    if second_expected is not None and most_common_indent == second_expected:
        return "second"
    return "others"

def find_blocks_in_file(path: str) -> List[Tuple[int, int, str, str]]:
    """Return list of (start_line, first_star_idx, block_text, opening_line) for every /** ... */ block.
    This version includes the original leading indentation before the '/**' so examples
    printed later preserve the exact source lines.
    """
    with open(path, "r", encoding="utf-8", errors="ignore") as f:
        text = f.read()
    results = []
    for m in BLOCK_RE.finditer(text):
        start_pos = m.start()
        # compute line number (1-based)
        start_line = text.count("\n", 0, start_pos) + 1
        # include leading indentation from the start of the line
        line_start = text.rfind("\n", 0, start_pos) + 1
        block_with_indent = text[line_start:m.end()]
        # opening line is the first line of the block_with_indent
        opening_line = block_with_indent.splitlines()[0] if block_with_indent.splitlines() else ""
        # get index of first '*' in opening line (relative to the line start)
        first_star_idx = opening_line.find("*")
        # normalise first_star_idx: if opening_line starts with spaces then '/', first_star_idx will be index of '*'.
        # keep as-is
        results.append((start_line, first_star_idx, block_with_indent if block_with_indent is not None else "", opening_line))
    return results

def walk_and_count(root: str) -> Tuple[Dict[str, int], List[Tuple[str,int,str,str]]]:
    """
    Walk directory tree and count styles.
    Returns (counts_by_category, details) where details is list of (file,start_line,category,block)
    categories: 'first', 'second', 'others'
    """
    counts: Dict[str, int] = {}
    details: List[Tuple[str,int,str,str]] = []
    for dirpath, dirnames, filenames in os.walk(root):
        # skip .git and target/build directories
        if ".git" in dirpath.split(os.sep):
            continue
        for fn in filenames:
            if not fn.endswith(SCALAR_EXTS):
                continue
            path = os.path.join(dirpath, fn)
            try:
                blocks = find_blocks_in_file(path)
            except Exception:
                continue
            for start_line, first_star_idx, block, opening_line in blocks:
                # skip one-line /** ... */ blocks (do not count)
                if "\n" not in block:
                    continue
                # determine opening second star idx if any (from opening_line)
                second_star_idx = (
                    opening_line.find("*", first_star_idx + 1) if first_star_idx != -1 else -1
                )
                second_star_idx = second_star_idx if second_star_idx != -1 else None

                category = classify_alignment(block, (first_star_idx, second_star_idx), opening_line)
                # treat None / unknown as 'others'
                if category is None:
                    category = "others"
                counts[category] = counts.get(category, 0) + 1
                details.append((path, start_line, category, block))
    return counts, details

def human_summary(counts: Dict[str,int]) -> str:
    lines = []
    order = ["first", "second", "others"]
    for k in order:
        if k in counts:
            lines.append(f"{k}: {counts[k]}")
    # include any other unexpected keys
    for k in sorted(set(counts.keys()) - set(order)):
        lines.append(f"{k}: {counts[k]}")
    return "\n".join(lines)

def main():
    p = argparse.ArgumentParser(description="Count Scaladoc styles by leading spaces before '*'.")
    p.add_argument("root", nargs="?", default=".", help="Root directory to scan (default: current dir)")
    p.add_argument("--list", action="store_true", help="List each found block with file and start line")
    p.add_argument("--examples", type=int, nargs="?", const=5, help="Show examples of 'others' category (default 5)")
    args = p.parse_args()

    counts, details = walk_and_count(args.root)
    print("Scaladoc style counts:")
    print(human_summary(counts))
    if args.list:
        print()
        print("Details (file : start_line : category):")
        for fp, ln, cat, _ in sorted(details, key=lambda x:(x[0], x[1])):
            print(f"{fp} : {ln} : {cat}")

    if args.examples:
        n = args.examples
        print()
        print(f"Examples of 'others' (showing up to {n}):")
        shown = 0
        for fp, ln, cat, block in details:
            if cat != "others":
                continue
            # print file, line and a trimmed block (max 20 lines)
            print(f"\n-- {fp} : {ln} --")
            lines = block.splitlines()
            # show up to first 20 lines of the block
            for L in lines[:20]:
                print(L)
            shown += 1
            if shown >= n:
                break

if __name__ == "__main__":
    main()