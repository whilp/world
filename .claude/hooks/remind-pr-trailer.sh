#!/bin/bash
# Reminds to add x-cosmic-pr-name trailer when committing on feature branches

set -euo pipefail

input=$(cat)

# Check if this is a commit-related prompt
if ! echo "$input" | jq -e '.user_prompt // empty' | grep -qi 'commit'; then
    exit 0
fi

cd "$CLAUDE_PROJECT_DIR"

# Check if we're on a feature branch (not main/master)
branch=$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo "")

if [[ "$branch" == "main" ]] || [[ "$branch" == "master" ]] || [[ -z "$branch" ]]; then
    exit 0
fi

# Check if there's a corresponding PR file for this branch
# Look for .github/pr/*.md files that might match
pr_files=$(ls -1 .github/pr/*.md 2>/dev/null | wc -l)

if [[ "$pr_files" -gt 0 ]]; then
    echo "HINT: If this commit is part of a PR, add the x-cosmic-pr-name trailer:" >&2
    echo "" >&2
    echo "  x-cosmic-pr-name: your-pr-file.md" >&2
    echo "" >&2
    echo "Available PR files in .github/pr/:" >&2
    ls -1 .github/pr/*.md | tail -5 | xargs -n1 basename >&2
fi

exit 0
