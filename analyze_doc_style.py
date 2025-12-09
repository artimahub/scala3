#!/usr/bin/env python3
"""
Analyze scaladoc comment style consistency across the Scala standard library.
"""

import re
from pathlib import Path
from collections import Counter, defaultdict
import sys

class ScaladocComment:
    """Represents a parsed scaladoc comment."""

    def __init__(self, text, file_path, line_number, element_name, element_type):
        self.text = text
        self.file_path = file_path
        self.line_number = line_number
        self.element_name = element_name
        self.element_type = element_type

        # Parse the comment
        self.lines = [line.strip() for line in text.split('\n')]
        self.first_sentence = self._extract_first_sentence()
        self.tags = self._extract_tags()
        self.has_example = '@example' in text or '{{{' in text
        self.has_code_blocks = '{{{' in text
        self.length = len(text)
        self.line_count = len([l for l in self.lines if l])

    def _extract_first_sentence(self):
        """Extract the first sentence from the comment."""
        # Remove tag lines
        content_lines = []
        for line in self.lines:
            if line.startswith('@'):
                break
            # Remove /** and */ and leading *
            line = re.sub(r'^\s*/\*\*\s*', '', line)
            line = re.sub(r'^\s*\*\s*', '', line)
            line = re.sub(r'\s*\*/\s*$', '', line)
            if line:
                content_lines.append(line)

        if not content_lines:
            return ""

        # Join lines and extract first sentence
        full_text = ' '.join(content_lines)
        # Try to find first sentence (ending with . ! or ?)
        match = re.match(r'^([^.!?]+[.!?])', full_text)
        if match:
            return match.group(1).strip()
        # If no sentence ending, return first line or first 100 chars
        return content_lines[0][:100] if content_lines else ""

    def _extract_tags(self):
        """Extract all scaladoc tags."""
        tags = []
        for line in self.lines:
            match = re.match(r'@(\w+)', line.strip())
            if match:
                tags.append(match.group(1))
        return tags

    def get_first_word(self):
        """Get the first word of the first sentence."""
        if not self.first_sentence:
            return ""
        words = self.first_sentence.split()
        return words[0] if words else ""

    def starts_with_verb(self):
        """Check if first sentence starts with a verb (common patterns)."""
        first_word = self.get_first_word().lower()
        # Common verb patterns in scaladoc
        verbs = {'returns', 'return', 'creates', 'create', 'gets', 'get', 'sets', 'set',
                 'converts', 'convert', 'checks', 'check', 'tests', 'test',
                 'adds', 'add', 'removes', 'remove', 'computes', 'compute',
                 'builds', 'build', 'constructs', 'construct', 'produces', 'produce',
                 'yields', 'yield', 'generates', 'generate', 'extracts', 'extract'}
        return first_word in verbs

    def is_complete_sentence(self):
        """Check if first sentence ends with proper punctuation."""
        return self.first_sentence.endswith('.') or self.first_sentence.endswith('!') or self.first_sentence.endswith('?')

def extract_scaladocs(file_path):
    """Extract all scaladoc comments from a Scala file."""
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
    except Exception as e:
        print(f"Error reading {file_path}: {e}", file=sys.stderr)
        return []

    scaladocs = []

    # Find all scaladoc comments (/** ... */)
    # Match with the next def/val/var/class/trait/object
    pattern = r'/\*\*(?P<doc>.*?)\*/\s*(?:@\w+\s+)*(?P<type>def|val|var|class|trait|object|enum)\s+(?P<name>\w+)'

    for match in re.finditer(pattern, content, re.DOTALL):
        doc_text = match.group('doc')
        element_type = match.group('type')
        element_name = match.group('name')

        # Find line number
        line_number = content[:match.start()].count('\n') + 1

        scaladocs.append(ScaladocComment(
            text='/**' + doc_text + '*/',
            file_path=file_path,
            line_number=line_number,
            element_name=element_name,
            element_type=element_type
        ))

    return scaladocs

def analyze_scaladocs(scaladocs):
    """Analyze scaladoc comments for style patterns."""

    stats = {
        'total': len(scaladocs),
        'by_element_type': Counter(),
        'first_words': Counter(),
        'verb_tenses': Counter(),
        'with_examples': 0,
        'with_params': 0,
        'with_returns': 0,
        'with_throws': 0,
        'with_see': 0,
        'with_since': 0,
        'with_deprecated': 0,
        'complete_sentences': 0,
        'starts_with_verb': 0,
        'short_docs': 0,  # < 50 chars
        'medium_docs': 0,  # 50-200 chars
        'long_docs': 0,  # > 200 chars
        'single_line': 0,
        'multi_line': 0,
        'tag_usage': Counter(),
    }

    examples = {
        'imperative_present': [],  # "Returns the..."
        'imperative_no_s': [],  # "Return the..."
        'descriptive': [],  # "The result of..."
        'noun_phrase': [],  # "A builder for..."
        'incomplete': [],  # No ending punctuation
        'with_example': [],
        'with_multiple_tags': [],
        'very_short': [],
        'very_long': [],
    }

    for doc in scaladocs:
        # Element type
        stats['by_element_type'][doc.element_type] += 1

        # First word
        first_word = doc.get_first_word().lower()
        if first_word:
            stats['first_words'][first_word] += 1

        # Verb tense analysis
        if first_word in ['returns', 'creates', 'gets', 'sets', 'converts', 'checks',
                          'adds', 'removes', 'computes', 'builds', 'produces', 'generates']:
            stats['verb_tenses']['third_person_singular'] += 1
            if len(examples['imperative_present']) < 5:
                examples['imperative_present'].append((doc.file_path, doc.line_number, doc.first_sentence))
        elif first_word in ['return', 'create', 'get', 'set', 'convert', 'check',
                            'add', 'remove', 'compute', 'build', 'produce', 'generate']:
            stats['verb_tenses']['base_form'] += 1
            if len(examples['imperative_no_s']) < 5:
                examples['imperative_no_s'].append((doc.file_path, doc.line_number, doc.first_sentence))
        elif first_word in ['the', 'a', 'an']:
            stats['verb_tenses']['noun_phrase'] += 1
            if len(examples['noun_phrase']) < 5:
                examples['noun_phrase'].append((doc.file_path, doc.line_number, doc.first_sentence))
        elif first_word and not doc.starts_with_verb():
            if len(examples['descriptive']) < 5:
                examples['descriptive'].append((doc.file_path, doc.line_number, doc.first_sentence))

        # Tags
        if 'example' in doc.tags or doc.has_example:
            stats['with_examples'] += 1
            if len(examples['with_example']) < 5:
                examples['with_example'].append((doc.file_path, doc.line_number, doc.element_name))

        for tag in doc.tags:
            stats['tag_usage'][tag] += 1

        if 'param' in doc.tags:
            stats['with_params'] += 1
        if 'return' in doc.tags:
            stats['with_returns'] += 1
        if 'throws' in doc.tags:
            stats['with_throws'] += 1
        if 'see' in doc.tags:
            stats['with_see'] += 1
        if 'since' in doc.tags:
            stats['with_since'] += 1
        if 'deprecated' in doc.tags:
            stats['with_deprecated'] += 1

        if len(doc.tags) >= 3:
            if len(examples['with_multiple_tags']) < 5:
                examples['with_multiple_tags'].append((doc.file_path, doc.line_number, doc.element_name))

        # Sentence structure
        if doc.is_complete_sentence():
            stats['complete_sentences'] += 1
        else:
            if len(examples['incomplete']) < 5:
                examples['incomplete'].append((doc.file_path, doc.line_number, doc.first_sentence))

        if doc.starts_with_verb():
            stats['starts_with_verb'] += 1

        # Length analysis
        if doc.length < 50:
            stats['short_docs'] += 1
            if len(examples['very_short']) < 3:
                examples['very_short'].append((doc.file_path, doc.line_number, doc.first_sentence))
        elif doc.length < 200:
            stats['medium_docs'] += 1
        else:
            stats['long_docs'] += 1
            if len(examples['very_long']) < 3:
                examples['very_long'].append((doc.file_path, doc.line_number, doc.element_name))

        if doc.line_count <= 1:
            stats['single_line'] += 1
        else:
            stats['multi_line'] += 1

    return stats, examples

def print_report(stats, examples):
    """Print analysis report."""

    total = stats['total']

    print("=" * 80)
    print("SCALADOC STYLE ANALYSIS REPORT")
    print("=" * 80)
    print(f"\nTotal scaladoc comments analyzed: {total}\n")

    print("=" * 80)
    print("1. DOCUMENTATION BY ELEMENT TYPE")
    print("=" * 80)
    for elem_type, count in stats['by_element_type'].most_common():
        pct = 100 * count / total
        print(f"  {elem_type:10s}: {count:5d} ({pct:5.1f}%)")

    print("\n" + "=" * 80)
    print("2. VERB TENSE AND STYLE INCONSISTENCIES")
    print("=" * 80)

    third_person = stats['verb_tenses'].get('third_person_singular', 0)
    base_form = stats['verb_tenses'].get('base_form', 0)
    noun_phrase = stats['verb_tenses'].get('noun_phrase', 0)

    print(f"\nThird-person singular (Returns, Creates, etc.): {third_person} ({100*third_person/total:.1f}%)")
    print(f"Base form imperative (Return, Create, etc.):    {base_form} ({100*base_form/total:.1f}%)")
    print(f"Noun phrases (The..., A..., etc.):              {noun_phrase} ({100*noun_phrase/total:.1f}%)")

    print("\n** INCONSISTENCY: Mix of verb tenses/styles **")
    print("   Recommendation: Standardize on third-person singular (e.g., 'Returns')")

    print("\nExamples of third-person singular style:")
    for path, line, sentence in examples['imperative_present'][:3]:
        rel_path = Path(path).relative_to(Path('library/src')) if 'library/src' in str(path) else path
        print(f"  {rel_path}:{line}")
        print(f"    \"{sentence[:80]}...\"" if len(sentence) > 80 else f"    \"{sentence}\"")

    print("\nExamples of base form style:")
    for path, line, sentence in examples['imperative_no_s'][:3]:
        rel_path = Path(path).relative_to(Path('library/src')) if 'library/src' in str(path) else path
        print(f"  {rel_path}:{line}")
        print(f"    \"{sentence[:80]}...\"" if len(sentence) > 80 else f"    \"{sentence}\"")

    print("\n" + "=" * 80)
    print("3. SENTENCE COMPLETENESS")
    print("=" * 80)
    complete = stats['complete_sentences']
    incomplete = total - complete
    print(f"\nComplete sentences (ending with ./?/!): {complete} ({100*complete/total:.1f}%)")
    print(f"Incomplete sentences:                    {incomplete} ({100*incomplete/total:.1f}%)")

    if incomplete > total * 0.1:
        print("\n** INCONSISTENCY: Many incomplete sentences **")
        print("   Recommendation: Ensure all documentation ends with proper punctuation")

    print("\n" + "=" * 80)
    print("4. TAG USAGE")
    print("=" * 80)
    print(f"\nWith @example:    {stats['with_examples']} ({100*stats['with_examples']/total:.1f}%)")
    print(f"With @param:      {stats['with_params']} ({100*stats['with_params']/total:.1f}%)")
    print(f"With @return:     {stats['with_returns']} ({100*stats['with_returns']/total:.1f}%)")
    print(f"With @throws:     {stats['with_throws']} ({100*stats['with_throws']/total:.1f}%)")
    print(f"With @see:        {stats['with_see']} ({100*stats['with_see']/total:.1f}%)")
    print(f"With @since:      {stats['with_since']} ({100*stats['with_since']/total:.1f}%)")
    print(f"With @deprecated: {stats['with_deprecated']} ({100*stats['with_deprecated']/total:.1f}%)")

    print("\n** INCONSISTENCY: Low usage of @example, @throws, @see tags **")
    print("   Recommendation: Add examples to complex methods, document exceptions")

    print("\nAll tags used (top 15):")
    for tag, count in stats['tag_usage'].most_common(15):
        print(f"  @{tag:15s}: {count:5d}")

    print("\n" + "=" * 80)
    print("5. DOCUMENTATION LENGTH")
    print("=" * 80)
    short = stats['short_docs']
    medium = stats['medium_docs']
    long = stats['long_docs']

    print(f"\nShort (< 50 chars):     {short} ({100*short/total:.1f}%)")
    print(f"Medium (50-200 chars):  {medium} ({100*medium/total:.1f}%)")
    print(f"Long (> 200 chars):     {long} ({100*long/total:.1f}%)")

    print(f"\nSingle-line:  {stats['single_line']} ({100*stats['single_line']/total:.1f}%)")
    print(f"Multi-line:   {stats['multi_line']} ({100*stats['multi_line']/total:.1f}%)")

    print("\n" + "=" * 80)
    print("6. MOST COMMON STARTING WORDS")
    print("=" * 80)
    print("\nTop 20 words used to start documentation:")
    for word, count in stats['first_words'].most_common(20):
        print(f"  {word:15s}: {count:5d}")

    print("\n" + "=" * 80)
    print("7. KEY RECOMMENDATIONS FOR CONSISTENCY")
    print("=" * 80)
    print("""
1. VERB TENSE: Standardize on third-person singular present tense
   - Use: "Returns", "Creates", "Computes"
   - Avoid: "Return", "Create", "Compute"

2. SENTENCE STRUCTURE: Always use complete sentences with proper punctuation
   - Good: "Returns the sum of two numbers."
   - Bad: "The sum of two numbers"

3. TAGS: Increase usage of documentation tags where appropriate
   - Add @example for complex methods (currently only {:.1f}%)
   - Add @throws for methods that throw exceptions (currently only {:.1f}%)
   - Add @param descriptions for all parameters
   - Add @return descriptions for non-obvious return values

4. LENGTH: Expand terse documentation with more context
   - {:.0f}% of docs are very short (< 50 chars)
   - Consider adding usage examples and edge case descriptions

5. EXAMPLES: Add code examples to demonstrate usage
   - Currently only {:.1f}% of methods have examples
   - Especially important for complex APIs
""".format(
        100*stats['with_examples']/total,
        100*stats['with_throws']/total,
        100*stats['short_docs']/total,
        100*stats['with_examples']/total
    ))

def main():
    library_path = Path('library/src/scala')

    if not library_path.exists():
        print(f"Error: {library_path} does not exist", file=sys.stderr)
        sys.exit(1)

    # Find all Scala files
    scala_files = list(library_path.rglob('*.scala'))
    print(f"Analyzing scaladoc comments in {len(scala_files)} files...\n", file=sys.stderr)

    all_scaladocs = []
    for filepath in scala_files:
        scaladocs = extract_scaladocs(filepath)
        all_scaladocs.extend(scaladocs)

    print(f"Found {len(all_scaladocs)} scaladoc comments\n", file=sys.stderr)

    # Analyze
    stats, examples = analyze_scaladocs(all_scaladocs)

    # Print report
    print_report(stats, examples)

if __name__ == '__main__':
    main()
