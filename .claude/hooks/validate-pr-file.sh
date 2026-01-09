#!/bin/bash
# Validates .github/pr/*.md files follow the expected format

set -euo pipefail

input=$(cat)

file_path=$(echo "$input" | jq -r '.file_path // empty' 2>/dev/null || echo "")

# Only check .github/pr/*.md files
if [[ ! "$file_path" =~ \.github/pr/.*\.md$ ]]; then
    exit 0
fi

content=$(echo "$input" | jq -r '.content // .new_string // empty' 2>/dev/null || echo "")

errors=""

# Check for title in format: # component: verb explanation
if ! echo "$content" | head -1 | grep -qE '^# [a-z0-9_/-]+: [a-z]'; then
    errors="${errors}ERROR: PR title must follow '# component: verb explanation' format\n"
    errors="${errors}Example: '# cosmic: add bootstrap skill'\n"
fi

# Check for Validation section
if ! echo "$content" | grep -q '## Validation'; then
    errors="${errors}WARNING: PR file should include '## Validation' section with checklist\n"
fi

if [[ -n "$errors" ]]; then
    echo -e "$errors" >&2
    exit 2  # Block
fi

exit 0
