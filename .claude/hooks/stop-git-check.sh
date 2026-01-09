#!/bin/bash
# Checks for uncommitted changes before ending session

set -euo pipefail

cd "$CLAUDE_PROJECT_DIR"

# Check if there are uncommitted changes
if ! git diff-index --quiet HEAD -- 2>/dev/null; then
    echo "WARNING: You have uncommitted changes. Consider committing before ending session." >&2
    echo "Run 'git status' to see changes." >&2
    # Don't block, just warn (exit 0)
fi

# Check for untracked files if status.showUntrackedFiles is set
untracked=$(git ls-files --others --exclude-standard)
if [[ -n "$untracked" ]]; then
    echo "INFO: You have untracked files:" >&2
    echo "$untracked" | head -5 >&2
fi

exit 0
