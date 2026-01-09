#!/bin/bash
# Verifies build dependencies before running make test

set -euo pipefail

input=$(cat)

command=$(echo "$input" | jq -r '.command // empty' 2>/dev/null || echo "")

# Only check 'make test' commands
if [[ ! "$command" =~ make.*test ]]; then
    exit 0
fi

# Check if we're in the project directory
if [[ ! -f "$CLAUDE_PROJECT_DIR/Makefile" ]]; then
    exit 0
fi

cd "$CLAUDE_PROJECT_DIR"

# Quick check: if o/ doesn't exist, suggest make staged
if [[ ! -d "o" ]]; then
    echo "HINT: Run 'make staged' first to fetch and extract dependencies" >&2
    # Don't block, just warn
fi

exit 0
