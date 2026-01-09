#!/bin/bash
# Automatically runs 'cosmic --skill pr' after git commits with x-cosmic-pr-name trailer

set -euo pipefail

input=$(cat)

# Only run after successful git commit
tool_name=$(echo "$input" | jq -r '.tool_name // empty' 2>/dev/null || echo "")
command=$(echo "$input" | jq -r '.command // empty' 2>/dev/null || echo "")

if [[ "$tool_name" != "Bash" ]] || [[ ! "$command" =~ git[[:space:]]+commit ]]; then
    exit 0
fi

cd "$CLAUDE_PROJECT_DIR"

# Check if the last commit has x-cosmic-pr-name trailer
trailer=$(git log -1 --format='%(trailers:key=x-cosmic-pr-name,valueonly)' 2>/dev/null || echo "")

if [[ -n "$trailer" ]]; then
    echo "INFO: Found x-cosmic-pr-name trailer, updating PR..." >&2

    # Run cosmic --skill pr if we have the required env vars
    if [[ -n "${GITHUB_TOKEN:-}" ]] && [[ -n "${GITHUB_PR_NUMBER:-}" ]]; then
        "$CLAUDE_PROJECT_DIR/bin/cosmic" --skill pr || echo "WARNING: Failed to update PR" >&2
    else
        echo "HINT: Run 'bin/cosmic --skill pr' manually to update the PR (requires GITHUB_TOKEN and GITHUB_PR_NUMBER)" >&2
    fi
fi

exit 0
