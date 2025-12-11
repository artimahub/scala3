#!/usr/bin/env python3
"""
Scaladoc checker

Usage:
  python tools/scaladoc_checker.py /path/to/scala/project

This script recursively scans .scala files under the given folder and validates
Scaladoc tags (@param, @tparam, @return) against the following heuristic rules:
- @param tags must reference existing parameter names in the next declaration.
- @tparam tags must reference existing type parameter names.
- @return should be present if the declaration has a non-Unit return type (for defs),
  and should be absent for Unit-returning defs. For constructors/classes return is ignored.

Output: text report to stdout and exit code 0.
"""
from __future__ import annotations
import argparse
import os
import re
import sys
import json
from typing import List, Tuple, Dict, Optional

SCALADOC_RE = re.compile(r"/\*\*(.*?)\*/", re.DOTALL)
TAG_RE = re.compile(r"^\s*\*\s*@(\w+)\s*(\w+)?", re.MULTILINE)
# Heuristic patterns for declarations
DECL_START_RE = re.compile(r"^\s*(?:@[\w\(\)\s]+)?\s*(?:private|protected|final|override|inline|implicit|given|export|opaque|sealed|case\s+)?\s*(class|trait|object|def|val|var)\b")
DEF_RE = re.compile(r"^\s*(?:def)\s+([^\s\(\[]+)\s*(?:\[(.*?)\])?\s*(\([^)]*\))?", re.S)
CLASS_RE = re.compile(r"^\s*(?:class|trait)\s+([^\s\(\[]+)\s*(?:\[(.*?)\])?\s*(\([^)]*\))?", re.S)

def find_scala_files(root: str) -> List[str]:
    scala_files = []
    for dirpath, _, filenames in os.walk(root):
        for fn in filenames:
            if fn.endswith(".scala"):
                scala_files.append(os.path.join(dirpath, fn))
    return scala_files

def extract_scaladoc_blocks(text: str) -> List[Tuple[int, int, str]]:
    """
    Return list of (start_index, end_index, content) for each /** ... */ block.
    start_index and end_index are string offsets.
    """
    return [(m.start(), m.end(), m.group(1)) for m in SCALADOC_RE.finditer(text)]

def extract_tags(block: str) -> Dict[str, List[str]]:
    """
    Extract @param, @tparam, @return tags. For @return we store an empty-string-name key.
    """
    tags: Dict[str, List[str]] = {"param": [], "tparam": [], "return": []}
    for m in TAG_RE.finditer(block):
        tag = m.group(1)
        name = m.group(2) or ""
        if tag == "param":
            tags["param"].append(name)
        elif tag == "tparam":
            tags["tparam"].append(name)
        elif tag == "return":
            # @return may have no name
            tags["return"].append("")
    return tags

def get_declaration_after(text: str, pos: int) -> Tuple[str, int]:
    """
    Given text and index pos (end of scaladoc block), return the following declaration chunk
    (up to next '{' or '=' or '\n\n' or up to 20 lines) and the line number where it starts.
    """
    # get slice after pos
    tail = text[pos:]
    # find start line number
    start_line = text[:pos].count("\n") + 1
    lines = tail.splitlines()
    # skip blank and annotation lines that may appear immediately after Scaladoc
    chunk_lines = []
    for i, line in enumerate(lines[:40]):  # limit lookahead to 40 lines
        if i == 0 and line.strip() == "":
            continue
        chunk_lines.append(line)
        # Heuristic: stop when we see an opening brace for class/object, or '=' for defs, or end of signature ')'
        if "{" in line or "=" in line or line.strip().endswith(")") or line.strip().endswith(":"):
            break
    chunk = "\n".join(chunk_lines)
    return chunk, start_line

def parse_declaration(chunk: str) -> Dict[str, object]:
    """
    Heuristic parsing of chunk to extract:
    - kind: 'def', 'class', 'trait', 'object', 'val', 'var' or 'unknown'
    - name: identifier (best-effort)
    - tparams: list of type parameter names
    - params: list of parameter names
    - return: return type string or None
    """
    res = {"kind": "unknown", "name": "", "tparams": [], "params": [], "return": None}
    first_line = chunk.strip().splitlines()[0] if chunk.strip() else ""
    kind_match = DECL_START_RE.search(first_line)
    if not kind_match:
        # try whole chunk
        kind_match = DECL_START_RE.search(chunk)
    if not kind_match:
        return res
    kind = kind_match.group(1)
    res["kind"] = kind
    # For def:
    if kind == "def":
        m = DEF_RE.search(chunk)
        if m:
            name = m.group(1)
            res["name"] = name
            tps = m.group(2) or ""
            if tps:
                res["tparams"] = [tp.strip().split(":")[0].split("=")[0].strip() for tp in tps.split(",")]
            params_str = m.group(3) or ""
            res["params"] = extract_param_names_from_parens(params_str)
        # attempt to find return type
        # find ":" after parameter list
        after_params = chunk[m.end():] if m else chunk
        ret = None
        # Look for pattern : Type = or : Type { or : Type =
        ret_match = re.search(r":\s*([^=\{\n]+)", chunk)
        if ret_match:
            ret = ret_match.group(1).strip()
            # cleanup trailing tokens
            ret = re.split(r"\s+(?:=|\{)", ret)[0].strip()
            res["return"] = ret
        return res
    # For class/trait: extract type params and primary constructor params
    if kind in ("class", "trait"):
        m = CLASS_RE.search(chunk)
        if m:
            name = m.group(1)
            res["name"] = name
            tps = m.group(2) or ""
            if tps:
                res["tparams"] = [tp.strip().split(":")[0].split("=")[0].strip() for tp in tps.split(",")]
            params_str = m.group(3) or ""
            res["params"] = extract_param_names_from_parens(params_str)
        return res
    # For val/var: no tparams, but may have type annotation
    if kind in ("val", "var"):
        # find pattern val name: Type = ...
        m = re.search(r"\b(val|var)\s+([A-Za-z0-9_]+)", chunk)
        if m:
            res["name"] = m.group(2)
        return res
    # object: name only
    if kind == "object":
        m = re.search(r"\bobject\s+([A-Za-z0-9_]+)", chunk)
        if m:
            res["name"] = m.group(1)
        return res
    return res

def extract_param_names_from_parens(parens: str) -> List[str]:
    """
    Given a parenthesis group like "(x: Int, y: String)(z: Double)" return list of param names.
    """
    if not parens:
        return []
    names: List[str] = []
    # Find all parenthesis groups
    groups = re.findall(r"\(([^)]*)\)", parens)
    for g in groups:
        parts = split_by_commas_top_level(g)
        for p in parts:
            p = p.strip()
            if not p:
                continue
            # param form: name: Type = default or implicit name: Type
            # For patterns like (using ctx: Context), or (implicit ev: Eq[T])
            # consider name before ':' or if no ':', word before space
            m = re.match(r"([A-Za-z0-9_]+)\s*:", p)
            if m:
                names.append(m.group(1))
            else:
                # fallback: first token
                tok = p.split()[0]
                # ignore implicit/using keywords at start
                if tok in ("implicit", "using"):
                    rest = p.split()
                    if len(rest) > 1:
                        names.append(rest[1])
                else:
                    names.append(tok)
    return names

def split_by_commas_top_level(s: str) -> List[str]:
    """
    Split by commas but ignore commas inside brackets/parentheses.
    """
    parts = []
    cur = []
    depth = 0
    for ch in s:
        if ch in "([{":
            depth += 1
            cur.append(ch)
        elif ch in ")]}":
            depth = max(0, depth-1)
            cur.append(ch)
        elif ch == "," and depth == 0:
            parts.append("".join(cur))
            cur = []
        else:
            cur.append(ch)
    last = "".join(cur).strip()
    if last:
        parts.append(last)
    return parts

def analyze_file(path: str, fix: bool = False) -> Dict[str, object]:
    """
    Analyze a single file for scaladoc issues. If fix=True, also insert missing
    @tparam/@param/@return tags into the scaladoc blocks in the file.
    """
    with open(path, "r", encoding="utf-8") as f:
        text = f.read()

    results = []
    # We'll build a modified version of the text if --fix is requested.
    new_text = text
    offset = 0  # difference between new_text and original text indices when replacing
    for m in SCALADOC_RE.finditer(text):
        start, end = m.start(), m.end()
        block = m.group(1)
        tags = extract_tags(block)
        chunk, line = get_declaration_after(text, end)
        decl = parse_declaration(chunk)
        issues = []
        # Validate tparam
        declared_tparams = [t for t in decl.get("tparams", []) if t]
        documented_tparams = tags.get("tparam", [])
        missing_tparam = [t for t in declared_tparams if t not in documented_tparams]
        extra_tparam = [t for t in documented_tparams if t not in declared_tparams]
        if missing_tparam:
            issues.append(f"Missing @tparam for: {', '.join(missing_tparam)}")
        if extra_tparam:
            issues.append(f"@tparam refers to unknown type params: {', '.join(extra_tparam)}")
        # Validate param
        declared_params = decl.get("params", [])
        documented_params = tags.get("param", [])
        missing_param = [p for p in declared_params if p not in documented_params]
        extra_param = [p for p in documented_params if p not in declared_params]
        if missing_param:
            issues.append(f"Missing @param for: {', '.join(missing_param)}")
        if extra_param:
            issues.append(f"@param refers to unknown params: {', '.join(extra_param)}")
        # Validate return
        documented_return = len(tags.get("return", [])) > 0
        ret = decl.get("return")
        if decl.get("kind") == "def":
            # if return is Unit (or empty) -> shouldn't have @return
            if ret:
                if ret.strip().startswith("Unit"):
                    if documented_return:
                        issues.append("@return present but return type is Unit")
                else:
                    if not documented_return:
                        issues.append("Missing @return for non-Unit return type")
            else:
                # unknown return: if no @return, skip
                pass

        # If requested, insert missing tags into the comment
        if fix:
            insert_lines: List[str] = []
            # Insert missing tparams first (store without leading '*')
            for tp in missing_tparam:
                insert_lines.append(f"@tparam {tp} TODO FILL IN TPARAM")
            # Then missing params
            for p in missing_param:
                insert_lines.append(f"@param {p} TODO FILL IN PARAM")
            # Then return if needed
            if decl.get("kind") == "def" and ret and not documented_return and not (ret.strip().startswith("Unit")):
                insert_lines.append(f"@return TODO FILL IN RETURN")
            if insert_lines:
                # Build new inner block preserving existing content formatting.
                inner = block
                inner_lines = inner.splitlines()
                preserved_lines = []
                for l in inner_lines:
                    # If the line uses the "*" prefix (typical scaladoc), preserve
                    # everything after the star (including any extra spacing) so we
                    # don't alter intentional alignment.
                    mstar = re.match(r"^\s*\*(.*)$", l)
                    if mstar:
                        suffix = mstar.group(1)  # may be empty or start with spaces
                        if suffix == "":
                            preserved_lines.append((True, ""))  # blank star line
                        else:
                            preserved_lines.append((True, suffix))  # keep suffix as-is
                    else:
                        # Content on same line as "/** ...": keep stripped content
                        preserved_lines.append((False, l.strip()))
                # Determine the exact whitespace prefix on the comment line (may be empty).
                line_start_idx = text.rfind("\n", 0, start)
                if line_start_idx == -1:
                    line_prefix = text[:start]
                else:
                    line_prefix = text[line_start_idx + 1:start]
                leading_ws = re.match(r"^\s*", line_prefix).group(0)
                # Build the new comment block WITHOUT duplicating the existing leading
                # whitespace (text[:start] already contains that). Put '/**' immediately
                # after the original prefix, then for subsequent lines emit the leading
                # whitespace explicitly so they align.
                parts = []
                parts.append("/**")
                for is_star, content in preserved_lines:
                    if is_star:
                        if content == "":
                            parts.append(leading_ws + " *")
                        else:
                            # content already contains any intentional extra spaces
                            parts.append(leading_ws + " *" + content)
                    else:
                        # originally on the opener line; emit as standard " * " line
                        parts.append(leading_ws + " * " + content)
                # Append inserted tag lines
                for ln in insert_lines:
                    parts.append(leading_ws + " * " + ln)
                # closing
                parts.append(leading_ws + " */")
                new_full = "\n".join(parts)
                # Do not add or remove trailing newlines here; keep the following text
                # unchanged so we do not introduce or remove blank lines.
                new_text = new_text[:start + offset] + new_full + new_text[end + offset:]
                # Update offset for subsequent replacements
                offset += len(new_full) - (end - start)

        # Package result
        results.append({
            "line": line,
            "decl": decl,
            "tags": tags,
            "issues": issues,
            "scaladoc_excerpt": block.strip().splitlines()[0:3]
        })

    # If we modified the text, write back to file
    if fix and new_text != text:
        try:
            with open(path, "w", encoding="utf-8") as f:
                f.write(new_text)
        except Exception as e:
            print(f"Error writing fixes to {path}: {e}", file=sys.stderr)

    return {"path": path, "results": results}

def summarize_reports(reports: List[Dict[str, object]]) -> int:
    total_files = len(reports)
    total_issues = 0
    # Track totals and how many declarations (per kind) have issues
    kind_totals = {"class": 0, "trait": 0, "def": 0}
    kind_issues = {"class": 0, "trait": 0, "def": 0}
    for r in reports:
        path = r["path"]
        file_issues = []
        for item in r["results"]:
            decl = item.get("decl", {})
            kind = decl.get("kind")
            # count totals for relevant kinds
            if kind in kind_totals:
                kind_totals[kind] += 1
            if item["issues"]:
                # record for per-file listing
                file_issues.append((item["line"], decl, item["issues"]))
                # total_issues counts individual issue messages (keeps previous behaviour)
                total_issues += len(item["issues"])
                # count declarations-with-issues per kind
                if kind in kind_issues:
                    kind_issues[kind] += 1
        if file_issues:
            print(f"\nFile: {path}")
            for ln, decl, issues in file_issues:
                kind = decl.get("kind")
                name = decl.get("name") or "<unknown>"
                print(f"  At line {ln}: {kind} {name}")
                for it in issues:
                    print(f"    - {it}")
    # Print aggregated summary with percentages
    print("\nSummary:")
    for k in ("class", "trait", "def"):
        total = kind_totals.get(k, 0)
        bad = kind_issues.get(k, 0)
        pct = (bad / total * 100.0) if total > 0 else 0.0
        label = "methods" if k == "def" else (k + "s")
        print(f"  {label.capitalize()}: {total}, with issues: {bad} ({pct:.1f}%)")
    print(f"\nScanned {total_files} files. Found {total_issues} issues.")
    return total_issues

def main():
    p = argparse.ArgumentParser(description="Scaladoc tag checker")
    p.add_argument("folder", help="Root folder to scan")
    p.add_argument("--json", action="store_true", help="Output full JSON report")
    p.add_argument("--fix", action="store_true", help="Insert missing tags into source files")
    args = p.parse_args()
    folder = args.folder
    if not os.path.isdir(folder):
        print("Error: folder not found", file=sys.stderr)
        sys.exit(2)
    files = find_scala_files(folder)
    reports = []
    for f in files:
        try:
            reports.append(analyze_file(f, args.fix))
        except Exception as e:
            print(f"Error analyzing {f}: {e}", file=sys.stderr)
    if args.json:
        print(json.dumps(reports, indent=2))
    issues = summarize_reports(reports)
    sys.exit(0 if issues == 0 else 1)

if __name__ == "__main__":
    main()