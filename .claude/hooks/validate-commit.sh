#!/bin/bash
# Validates commit messages follow '<component>: <action>' convention

set -euo pipefail

input=$(cat)

# Extract commit message if this is a git commit operation
message=$(echo "$input" | jq -r '.command // empty' 2>/dev/null | grep -oP '(?<=-m ")[^"]+' || echo "")

if [[ -z "$message" ]]; then
    exit 0  # Not a commit, allow
fi

# Check format: <component>: <action>
if ! echo "$message" | grep -qE '^[a-z0-9_-]+: [a-z]'; then
    echo "ERROR: Commit message must follow '<component>: <action>' format" >&2
    echo "Example: 'cosmic: add bootstrap skill'" >&2
    echo "Your message: $message" >&2
    exit 2  # Block
fi

exit 0
