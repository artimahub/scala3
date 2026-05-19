# Loop Plan: Automated TODO Fill-in with AI Review

## Overview

This document describes an automated workflow to:
1. Cherry-pick commits containing TODO placeholders from `feature-create-param-tparam-return-todos`
2. Have an AI fill in the TODOs with meaningful documentation
3. Have a separate AI review the work and provide feedback
4. Allow the first AI to refine based on feedback
5. Commit the completed documentation

## Final Design Decisions

- **Reviewer sees before version?** No - reviewer can use `git diff` if needed
- **Refinement iterations**: One cycle only (Writer → Reviewer → Writer → Commit)
- **Store reviewer JSON**: Yes, in `reviews/` directory for later audit
- **Human approval**: No, but `MAX_FILES` allows processing in batches (10, then 50, etc.)
- **Restartable**: Yes, script detects already-cherry-picked commits automatically

## Source and Target Branches

- **Source branch**: `feature-create-param-tparam-return-todos`
  - Contains commits with message: `"Todo-writer added TODOs for @param, @tparam, and @return tags."`
  - Each commit modifies one `.scala` file with TODO placeholders

- **Target branch**: `feature-fill-in-param-tparam-return-tags`
  - Where the filled-in documentation will be committed

## Files Created

```
todo-writer/
├── fill-todos-loop.sh      # Main script
├── fill-todos.log          # Log file (created on run)
├── prompts/
│   ├── writer-prompt.txt   # Initial fill-in prompt
│   ├── reviewer-prompt.txt # Review prompt (returns JSON)
│   └── refinement-prompt.txt # Incorporate feedback prompt
└── reviews/                # Stored reviewer JSON feedback
    ├── library_src_scala_Array.scala.json
    └── ...
```

## Loop Structure

```
┌─────────────────────────────────────────────────────────────────┐
│  For each commit in source branch with matching message:        │
│                                                                 │
│  1. Cherry-pick commit into target branch                       │
│  2. AI #1 (Writer): Fill in all TODOs in the file              │
│  3. AI #2 (Reviewer): Review and provide structured feedback    │
│  4. AI #1 (Writer): Incorporate feedback and refine             │
│  5. Amend commit with filled-in documentation                   │
│  6. Sleep for configurable duration                             │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

## Usage

```bash
# Process 10 files (default), 60 second pause between each
./fill-todos-loop.sh

# Process 50 files with 120 second pause
MAX_FILES=50 PAUSE_SECONDS=120 ./fill-todos-loop.sh

# Dry run - see what would be processed without making changes
DRY_RUN=true ./fill-todos-loop.sh

# Process all remaining files (use with caution)
MAX_FILES=9999 ./fill-todos-loop.sh
```

## Configuration Options

| Variable | Default | Description |
|----------|---------|-------------|
| `MAX_FILES` | 10 | Maximum files to process in this run |
| `PAUSE_SECONDS` | 60 | Seconds to wait between processing files |
| `DRY_RUN` | false | Don't make changes, just show what would be done |
| `SOURCE_BRANCH` | feature-create-param-tparam-return-todos | Branch with TODO commits |

**Note:** The script cherry-picks to whatever branch you're currently on. It will refuse to run if you're on the source branch.

## AI Roles

### AI #1: Writer
- **Mode**: Single-turn with `-p` (fresh context each file)
- **Tools**: Edit, Read, Glob, Grep
- **Task**: Read file, understand code, fill in TODO placeholders
- **Prompt**: `prompts/writer-prompt.txt`

### AI #2: Reviewer
- **Mode**: Single-turn with `-p --output-format json`
- **Tools**: Read, Bash (for git diff), Grep
- **Task**: Review documentation, return structured JSON feedback
- **Prompt**: `prompts/reviewer-prompt.txt`

### AI #1 Again: Refinement
- **Mode**: Single-turn with `-p` (fresh context)
- **Tools**: Edit, Read
- **Task**: Incorporate reviewer feedback
- **Prompt**: `prompts/refinement-prompt.txt` + reviewer JSON

## Feedback Format Specification

The Reviewer returns JSON in this format:

```json
{
  "overall_quality": "excellent|good|acceptable|needs_work",
  "issues": [
    {
      "line": 42,
      "tag_type": "@param|@tparam|@return",
      "tag_name": "parameterName",
      "severity": "error|warning|suggestion",
      "current_text": "the text that was written",
      "problem": "description of what's wrong or could be improved",
      "suggestion": "specific suggested replacement text"
    }
  ],
  "general_feedback": "Overall comments",
  "requires_changes": true|false
}
```

### Severity Levels

- **error**: Factually incorrect, misleading, or missing critical information
- **warning**: Technically correct but unclear, incomplete, or inconsistent
- **suggestion**: Minor stylistic improvements, optional enhancements

### Quality Levels

- **excellent**: No issues, documentation is clear, accurate, and complete
- **good**: Minor suggestions only, no errors or warnings
- **acceptable**: Some warnings but no errors, usable as-is
- **needs_work**: Has errors that must be fixed

## Commit Message Format

Each processed file gets a commit with this message:

```
Filled in @param, @tparam, and @return documentation.

Original TODO commit: <short-hash>
Reviewer quality rating: <excellent|good|acceptable|needs_work>

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>
```

## Error Handling

1. **Cherry-pick conflicts**: Skip the commit, log error, continue
2. **Writer AI failure**: Log error, reset file, skip
3. **Reviewer AI failure**: Continue with refinement anyway (no feedback)
4. **Refinement AI failure**: Keep writer's original work, commit anyway

## Output Files

### Log File: `fill-todos.log`

```
[2024-01-15 10:23:45] Starting fill-todos-loop.sh
[2024-01-15 10:23:45] Source branch: feature-create-param-tparam-return-todos
[2024-01-15 10:23:46] Processing commit 1/10: abc1234
[2024-01-15 10:23:46] File in commit: library/src/scala/Array.scala
[2024-01-15 10:24:02] Review quality: good, requires_changes: false
[2024-01-15 10:24:15] Summary: library/src/scala/Array.scala - Quality: good
...
```

### Review Files: `reviews/<filename>.json`

Stored for later human review. Filename is the path with `/` replaced by `_`.

## Token Usage Estimates

Per file:
- Writer initial: ~2-5k tokens
- Reviewer: ~1-2k tokens
- Refinement: ~1-2k tokens
- **Total per file: ~4-9k tokens**

For 1500 files: ~6-13M tokens total

Recommended approach:
1. Run 10 files, review results
2. Run 50 files, spot check
3. Run remaining in larger batches overnight
