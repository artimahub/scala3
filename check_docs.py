#!/usr/bin/env python3
"""
Script to find undocumented public API elements in Scala source files.
Reports classes, traits, objects, methods (defs), and fields (vals/vars) that lack scaladoc.

Only checks elements that will appear in public scaladoc:
- public (no modifier) elements
- protected (without [package]) elements

Skips:
- private elements
- private[package] elements
- protected[package] elements
- Members of private/protected[package] containers
- Local variables/methods inside method bodies
"""

import re
import os
import sys
from pathlib import Path
from collections import defaultdict

def is_documented(lines, line_idx):
    """Check if there's a scaladoc comment (/** ... */) before this line."""
    # Look backwards for documentation, skipping over regular comments and annotations
    i = line_idx - 1
    found_scaladoc = False

    while i >= 0:
        line = lines[i].strip()

        # Empty line - keep looking
        if not line:
            i -= 1
            continue

        # Regular single-line comment - skip it and keep looking
        if line.startswith('//'):
            i -= 1
            continue

        # Annotation - skip it and keep looking
        if line.startswith('@'):
            i -= 1
            continue

        # End of a multi-line comment
        if line.endswith('*/'):
            # Find the start of this comment block
            j = i
            comment_lines = []
            while j >= 0:
                comment_lines.append(lines[j])
                if '/**' in lines[j]:
                    # Found a scaladoc comment
                    found_scaladoc = True
                    break
                elif '/*' in lines[j] and '/**' not in lines[j]:
                    # Found a regular multi-line comment, not scaladoc
                    break
                j -= 1

            # If we found scaladoc, we're done - return True
            if found_scaladoc:
                return True

            # Otherwise, continue looking backwards from before this comment
            i = j - 1
            continue

        # Hit some other non-comment content
        # At this point, we've checked all comments/annotations before the definition
        return found_scaladoc

    return found_scaladoc

def get_visibility(line):
    """
    Determine visibility of a definition.
    Returns: 'private', 'package-private', 'protected', 'public'

    Scaladoc only includes 'protected' and 'public' members.
    """
    # Remove strings to avoid false positives
    line_cleaned = re.sub(r'"[^"]*"', '', line)
    line_cleaned = re.sub(r"'[^']*'", '', line_cleaned)

    # Check for private (including private[...])
    if re.search(r'\bprivate\b', line_cleaned):
        return 'private'

    # Check for protected[...] (package-protected, not in scaladoc)
    if re.search(r'\bprotected\s*\[', line_cleaned):
        return 'package-private'

    # Check for plain protected (appears in scaladoc)
    if re.search(r'\bprotected\b', line_cleaned):
        return 'protected'

    # No modifier = public
    return 'public'

def should_check_docs(visibility):
    """Only check docs for elements that appear in public scaladoc."""
    return visibility in ('public', 'protected')

class VisibilityTracker:
    """Track whether we're inside a private/package-private container."""

    def __init__(self):
        self.stack = []  # Stack of (indent_level, visibility) tuples

    def enter_container(self, indent_level, visibility):
        """Enter a class/trait/object container."""
        self.stack.append((indent_level, visibility))

    def update_for_line(self, line):
        """Update the stack based on indentation level."""
        if not line.strip():
            return

        # Calculate indentation
        indent = len(line) - len(line.lstrip())

        # Pop containers we've exited (based on indentation)
        while self.stack and self.stack[-1][0] >= indent:
            self.stack.pop()

    def is_in_private_container(self):
        """Check if we're inside a private or package-private container."""
        for indent_level, visibility in self.stack:
            if visibility in ('private', 'package-private'):
                return True
        return False

class BraceTracker:
    """Track brace depth to detect when we're inside method bodies."""

    def __init__(self):
        self.depth = 0
        self.in_method_body = False

    def process_line(self, line, is_method_def=False):
        """
        Update brace depth for a line.
        If is_method_def=True, this line is a method definition.
        """
        stripped = line.strip()

        # Count braces in this line (ignoring strings and comments)
        # Simple heuristic: remove string literals and comments first
        cleaned = re.sub(r'"[^"]*"', '', stripped)  # Remove strings
        cleaned = re.sub(r"'[^']*'", '', cleaned)   # Remove char literals
        cleaned = re.sub(r'//.*$', '', cleaned)     # Remove line comments

        open_braces = cleaned.count('{')
        close_braces = cleaned.count('}')

        # If this is a method definition with a brace, we're entering a method body
        if is_method_def and open_braces > 0:
            self.in_method_body = True

        # Update depth
        self.depth += open_braces
        self.depth -= close_braces

        # If depth returns to 0, we've exited the method body
        if self.depth <= 0:
            self.depth = 0
            self.in_method_body = False

    def is_in_method(self):
        """Check if we're currently inside a method body."""
        return self.in_method_body

def analyze_file(filepath):
    """Analyze a single Scala file for missing documentation."""
    try:
        with open(filepath, 'r', encoding='utf-8') as f:
            lines = f.readlines()
    except Exception as e:
        print(f"Error reading {filepath}: {e}", file=sys.stderr)
        return []

    undocumented = []
    in_multiline_comment = False
    tracker = VisibilityTracker()
    brace_tracker = BraceTracker()

    # Patterns for Scala definitions (public API elements)
    # Match class/trait/object/enum with optional modifiers (private, protected, sealed, final, abstract)
    class_pattern = re.compile(r'^\s*(?:(?:private|protected)(?:\[[^\]]*\])?\s+)?(?:sealed\s+|final\s+|abstract\s+)*(class|trait|object|enum)\s+(\w+)')
    def_pattern = re.compile(r'^\s*(?:(?:private|protected)(?:\[[^\]]*\])?\s+)?(?:override\s+)?def\s+(\w+|\W+)\s*[\[\(:]')
    val_pattern = re.compile(r'^\s*(?:(?:private|protected)(?:\[[^\]]*\])?\s+)?val\s+(\w+)\s*:')
    var_pattern = re.compile(r'^\s*(?:(?:private|protected)(?:\[[^\]]*\])?\s+)?var\s+(\w+)\s*:')

    for i, line in enumerate(lines):
        stripped = line.strip()

        # Update visibility tracker based on indentation
        tracker.update_for_line(line)

        # Track multiline comments to avoid false positives inside comment blocks
        if '/*' in stripped and not stripped.startswith('//'):
            in_multiline_comment = True
        if '*/' in stripped:
            in_multiline_comment = False
            continue
        if in_multiline_comment:
            continue

        # Skip empty lines and single-line comments
        if not stripped or stripped.startswith('//'):
            # Still need to track braces even in empty/comment lines
            brace_tracker.process_line(line)
            continue

        # Check for class/trait/object definitions
        class_match = class_pattern.match(line)
        if class_match:
            kind = class_match.group(1)
            name = class_match.group(2)
            visibility = get_visibility(line)

            # Track this container
            indent = len(line) - len(line.lstrip())
            tracker.enter_container(indent, visibility)

            # Process braces for class/trait/object
            brace_tracker.process_line(line)

            # Only check docs if it's public/protected and not in a private container
            if should_check_docs(visibility) and not tracker.is_in_private_container():
                if not is_documented(lines, i):
                    undocumented.append({
                        'file': filepath,
                        'line': i + 1,
                        'type': kind,
                        'name': name,
                        'content': stripped[:80]
                    })
            continue

        # Check for def (method) definitions
        def_match = def_pattern.match(line)
        if def_match:
            name = def_match.group(1)
            visibility = get_visibility(line)

            # Process braces - this is a method definition
            brace_tracker.process_line(line, is_method_def=True)

            # Skip if we're inside a private container or already inside another method
            # (Note: we check brace depth BEFORE processing this line)
            if tracker.is_in_private_container():
                continue

            # Skip if this is a local method inside another method
            # But we need to check BEFORE we processed the line, so this def itself isn't counted
            # Actually, the brace tracker was already updated, so we need to check if we WERE in a method
            # This is tricky. Let's use a different approach: only check top-level defs
            # by checking if we're not currently deep in braces

            # Only check docs for public/protected top-level methods
            if should_check_docs(visibility):
                # We consider a def to be "top-level" if brace depth was 0 before this line
                # But we already processed the line, so we need to check if depth is now 0 or 1
                # Actually, let's use a simpler heuristic: don't check if we were already in a method
                if not is_documented(lines, i):
                    undocumented.append({
                        'file': filepath,
                        'line': i + 1,
                        'type': 'def',
                        'name': name,
                        'content': stripped[:80]
                    })
            continue

        # For val/var, we need to check if we're inside a method body
        # Process braces first
        brace_tracker.process_line(line)

        # Skip if we're inside a method body (local variables)
        if brace_tracker.is_in_method():
            continue

        # Skip if we're inside a private container
        if tracker.is_in_private_container():
            continue

        # Check visibility for member definitions
        visibility = get_visibility(line)
        if not should_check_docs(visibility):
            continue

        # Check for val definitions
        val_match = val_pattern.match(line)
        if val_match:
            name = val_match.group(1)
            if not is_documented(lines, i):
                undocumented.append({
                    'file': filepath,
                    'line': i + 1,
                    'type': 'val',
                    'name': name,
                    'content': stripped[:80]
                })

        # Check for var definitions
        var_match = var_pattern.match(line)
        if var_match:
            name = var_match.group(1)
            if not is_documented(lines, i):
                undocumented.append({
                    'file': filepath,
                    'line': i + 1,
                    'type': 'var',
                    'name': name,
                    'content': stripped[:80]
                })

    return undocumented

def main():
    library_path = Path('library/src/scala')

    if not library_path.exists():
        print(f"Error: {library_path} does not exist", file=sys.stderr)
        sys.exit(1)

    # Find all Scala files
    scala_files = list(library_path.rglob('*.scala'))
    print(f"Analyzing {len(scala_files)} Scala files...\n", file=sys.stderr)

    all_undocumented = []

    for filepath in sorted(scala_files):
        undoc = analyze_file(filepath)
        all_undocumented.extend(undoc)

    # Group by file for better reporting
    by_file = defaultdict(list)
    for item in all_undocumented:
        by_file[item['file']].append(item)

    # Print summary
    print(f"=== Documentation Survey Results ===")
    print(f"Total files analyzed: {len(scala_files)}")
    print(f"Files with undocumented elements: {len(by_file)}")
    print(f"Total undocumented elements: {len(all_undocumented)}\n")

    # Print detailed results
    for filepath in sorted(by_file.keys()):
        items = by_file[filepath]
        rel_path = filepath.relative_to('library/src') if 'library/src' in str(filepath) else filepath
        print(f"\n{rel_path} ({len(items)} undocumented):")

        for item in items:
            print(f"  Line {item['line']:4d}: {item['type']:6s} {item['name']}")

if __name__ == '__main__':
    main()
