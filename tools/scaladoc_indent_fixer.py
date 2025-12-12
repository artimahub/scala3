#!/usr/bin/env python3
"""
Scaladoc indentation fixer

Usage:
  python tools/scaladoc_indent_fixer.py /path/to/scala/project [--fix] [--normalize]

This tool scans .scala files and checks Scaladoc comment blocks (/** ... */).
By default it only rewrites blocks where the leading column of the opener
('/**') does not match the leading column of the inner "* ..." lines,
which indicates a broken indentation. Use --normalize to normalize all
Scaladoc blocks to the conventional style:

    /**
     * first line
     * second line
     */

Use --fix to write changes to files; without --fix the script performs a dry run.
"""
from __future__ import annotations
import argparse
import os
import re
import sys
from typing import List, Tuple

SCALADOC_RE = re.compile(r"/\*\*(.*?)\*/", re.DOTALL)


def find_scala_files(root: str) -> List[str]:
    scala_files: List[str] = []
    for dirpath, _, filenames in os.walk(root):
        for fn in filenames:
            if fn.endswith(".scala"):
                scala_files.append(os.path.join(dirpath, fn))
    return scala_files


def analyze_and_fix_text(text: str, normalize_all: bool = False) -> Tuple[str, List[Tuple[int, str, str]]]:
    """
    Analyze a text and return (new_text, changes)

    changes is a list of tuples: (start_line, original_snippet_preview, new_snippet_preview)
    """
    new_text = text
    offset = 0
    changes: List[Tuple[int, str, str]] = []

    for m in SCALADOC_RE.finditer(text):
        start, end = m.start(), m.end()
        inner = m.group(1)
        # Determine original line start and leading whitespace before the '/**'
        line_start_idx = text.rfind("\n", 0, start)
        if line_start_idx == -1:
            line_prefix = text[:start]
            start_line = 1
        else:
            line_prefix = text[line_start_idx + 1:start]
            start_line = text[:start].count("\n") + 1
        leading_ws = re.match(r"^\s*", line_prefix).group(0)

        inner_lines = inner.splitlines()
        # Preserve single-line Scaladoc comments unchanged unless --normalize is requested.
        original_block_preview = text[start:end]
        if "\n" not in original_block_preview and not normalize_all:
            continue
        # Detect whether the original block ended with a star-only blank line (e.g. " *")
        original_had_trailing_star_blank = False
        for rev in reversed(inner_lines):
            mrev = re.match(r"^\s*\*(.*)$", rev)
            if mrev:
                if mrev.group(1).strip() == "":
                    original_had_trailing_star_blank = True
                break
            else:
                # If last line was a non-star blank line, treat it as trailing blank as well.
                if rev.strip() == "":
                    original_had_trailing_star_blank = True
                break

        # Collect leading whitespace before '*' on inner lines
        star_prefixes = set()
        for l in inner_lines:
            mstar = re.match(r"^(\s*)\*", l)
            if mstar:
                star_prefixes.add(mstar.group(1))

        # Decide whether to rewrite:
        # - If normalize_all => rewrite
        # - Else rewrite only when any star_prefix differs from leading_ws, or when there are no star lines
        need_rewrite = normalize_all or (len(star_prefixes) == 0) or any(p != leading_ws for p in star_prefixes)

        if not need_rewrite:
            continue

        # Build normalized block.
        parts: List[str] = []
        parts.append("/**")
        in_code_block = False
        in_pre_block = False

        def emit_preserve(c: str) -> None:
            # Emit a line preserving the exact content spacing after the star.
            # Ensure there is at least one space between '*' and content for readability:
            if c.startswith(" "):
                parts.append(leading_ws + " *" + c)
            else:
                parts.append(leading_ws + " * " + c)

        def emit_normalized(c: str) -> None:
            # Emit a normalized paragraph/list line: collapse leading spaces to one.
            cs = c.lstrip()
            if cs == "":
                parts.append(leading_ws + " *")
            else:
                parts.append(leading_ws + " * " + cs)

        for l in inner_lines:
            mstar = re.match(r"^\s*\*(.*)$", l)
            # content is the text after the '*' if present, otherwise the raw line (content on opener line).
            content = mstar.group(1) if mstar else l
            if content is None:
                content = ""
            stripped_for_fence = content.lstrip()
            # Detect triple-brace code fences and <pre> blocks and preserve spacing inside them.
            if not in_code_block and stripped_for_fence.startswith("{{{"):
                in_code_block = True
                emit_preserve(content)
                continue
            if in_code_block:
                emit_preserve(content)
                if "}}}" in content:
                    in_code_block = False
                continue
            if not in_pre_block and stripped_for_fence.lower().startswith("<pre>"):
                in_pre_block = True
                emit_preserve(content)
                continue
            if in_pre_block:
                emit_preserve(content)
                if "</pre>" in stripped_for_fence.lower():
                    in_pre_block = False
                continue
            # Preserve lines that start with one or more spaces after the star: these are often
            # code examples or caret markers whose exact column positions must be kept.
            if content.startswith(" "):
                emit_preserve(content)
            else:
                # Normal paragraph/list lines: collapse leading spaces after '*' to a single space
                emit_normalized(content)
        # closing - avoid introducing an artificial trailing blank star-line if the original
        # did not have one.
        if not original_had_trailing_star_blank and parts and parts[-1] == (leading_ws + " *"):
            parts.pop()
        parts.append(leading_ws + " */")
        new_block = "\n".join(parts)

        original_block = text[start:end]
        if new_block != original_block:
            # Apply replacement in new_text with offset
            new_text = new_text[: start + offset] + new_block + new_text[end + offset :]
            offset += len(new_block) - (end - start)
            # Prepare a short preview for reporting (first line of original and new)
            orig_preview = original_block.strip().splitlines()[0] if original_block.strip() else original_block[:40]
            new_preview = new_block.strip().splitlines()[0] if new_block.strip() else new_block[:40]
            changes.append((start_line, orig_preview, new_preview))

    return new_text, changes


def process_file(path: str, fix: bool, normalize_all: bool) -> Tuple[int, int]:
    """
    Process one file, optionally fix. Returns (num_blocks_examined, num_blocks_changed).
    """
    with open(path, "r", encoding="utf-8") as f:
        text = f.read()
    new_text, changes = analyze_and_fix_text(text, normalize_all=normalize_all)
    if changes and fix:
        with open(path, "w", encoding="utf-8") as f:
            f.write(new_text)
    return (len(list(SCALADOC_RE.finditer(text))), len(changes))


def main():
    p = argparse.ArgumentParser(description="Fix Scaladoc indentation")
    p.add_argument("folder", help="Root folder to scan")
    p.add_argument("--fix", action="store_true", help="Apply fixes to files")
    p.add_argument("--normalize", action="store_true", help="Normalize all scaladoc blocks, not only obviously broken ones")
    args = p.parse_args()

    folder = args.folder
    if not os.path.isdir(folder):
        print("Error: folder not found", file=sys.stderr)
        sys.exit(2)

    files = find_scala_files(folder)
    total_blocks = 0
    total_changed = 0
    changed_files = []

    for f in files:
        try:
            with open(f, "r", encoding="utf-8") as fh:
                text = fh.read()
            _, changes = analyze_and_fix_text(text, normalize_all=args.normalize)
            if changes:
                total_changed += len(changes)
                changed_files.append((f, changes))
            total_blocks += len(list(SCALADOC_RE.finditer(text)))
        except Exception as e:
            print(f"Error processing {f}: {e}", file=sys.stderr)

    # Report
    if not changed_files:
        print("No Scaladoc indentation issues detected.")
    else:
        print(f"Detected changes in {len(changed_files)} files, {total_changed} blocks affected:")
        for path, changes in changed_files:
            print(f"\nFile: {path}")
            for start_line, orig_preview, new_preview in changes:
                print(f"  At line {start_line}:")
                print(f"    - original: {orig_preview}")
                print(f"    - proposed: {new_preview}")

        if args.fix:
            # Now apply changes
            for path, _ in changed_files:
                try:
                    with open(path, "r", encoding="utf-8") as fh:
                        text = fh.read()
                    new_text, changes = analyze_and_fix_text(text, normalize_all=args.normalize)
                    if changes:
                        with open(path, "w", encoding="utf-8") as fh:
                            fh.write(new_text)
                        print(f"Applied fixes to {path} ({len(changes)} blocks).")
                except Exception as e:
                    print(f"Error writing {path}: {e}", file=sys.stderr)

    # Exit code: 0 if no problems (or fixes applied), 1 if issues found in dry-run
    if not args.fix and changed_files:
        sys.exit(1)
    sys.exit(0)


if __name__ == "__main__":
    main()