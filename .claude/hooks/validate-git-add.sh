#!/bin/bash
# Prevents 'git add -A' and enforces atomic commits

set -euo pipefail

input=$(cat)

command=$(echo "$input" | jq -r '.command // empty' 2>/dev/null || echo "")

if [[ "$command" =~ git[[:space:]]+add[[:space:]]+-A ]]; then
    echo "ERROR: Never use 'git add -A'. Add specific files for atomic commits." >&2
    echo "Use: git add path/to/specific/file.ext" >&2
    exit 2  # Block
fi

exit 0
