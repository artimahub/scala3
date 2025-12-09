#!/usr/bin/env python3
"""Summarize the documentation analysis results."""

import re
from collections import Counter, defaultdict
import sys

# Use the v2 file if it exists, otherwise use the original
input_file = 'docs_analysis_v2.txt' if len(sys.argv) < 2 else sys.argv[1]

# Parse the analysis file
with open(input_file, 'r') as f:
    lines = f.readlines()

# Extract statistics
total_files = 0
files_with_issues = 0
total_undoc = 0
element_types = Counter()
packages = Counter()
files_by_issue_count = []

for line in lines:
    if 'Total files analyzed:' in line:
        total_files = int(line.split(':')[1].strip())
    elif 'Files with undocumented elements:' in line:
        files_with_issues = int(line.split(':')[1].strip())
    elif 'Total undocumented elements:' in line:
        total_undoc = int(line.split(':')[1].strip())
    elif re.match(r'scala/.*\.scala \(\d+ undocumented\):', line):
        filepath = line.split('(')[0].strip()
        package = '/'.join(filepath.split('/')[:-1]) if '/' in filepath else 'scala'
        count = int(re.search(r'\((\d+) undocumented\)', line).group(1))
        packages[package] += count
        files_by_issue_count.append((filepath, count))
    elif re.match(r'\s+Line\s+\d+:\s+\w+', line):
        parts = line.split()
        if len(parts) >= 4:
            elem_type = parts[3]
            element_types[elem_type] += 1

print('=== Summary by Element Type ===')
for elem_type, count in element_types.most_common():
    print(f'{elem_type:10s}: {count:5d} undocumented')

print()
print('=== Summary by Package (Top 20) ===')
for pkg, count in sorted(packages.items(), key=lambda x: x[1], reverse=True)[:20]:
    print(f'{pkg:50s}: {count:5d} undocumented')

print()
print('=== Files with Most Issues (Top 30) ===')
for filepath, count in sorted(files_by_issue_count, key=lambda x: x[1], reverse=True)[:30]:
    print(f'{filepath:60s}: {count:4d} undocumented')

print()
print(f'=== Overall Statistics ===')
print(f'Files analyzed: {total_files}')
print(f'Files with issues: {files_with_issues} ({100*files_with_issues/total_files:.1f}%)')
print(f'Total undocumented: {total_undoc}')
