#!/bin/bash
# Validates lua files for anti-patterns before writing

set -euo pipefail

input=$(cat)

file_path=$(echo "$input" | jq -r '.file_path // empty' 2>/dev/null || echo "")
content=$(echo "$input" | jq -r '.content // .new_string // empty' 2>/dev/null || echo "")

# Only check .lua files
if [[ ! "$file_path" =~ \.lua$ ]]; then
    exit 0
fi

errors=""

# Check for os.execute
if echo "$content" | grep -q 'os\.execute'; then
    errors="${errors}ERROR: Never use os.execute() - use spawn module instead\n"
fi

# Check for io.popen
if echo "$content" | grep -q 'io\.popen'; then
    errors="${errors}ERROR: Never use io.popen() - use spawn module instead\n"
fi

# Check for string concatenation in paths (common pattern)
if echo "$content" | grep -qE '["'"'"'][^"'"'"']*\/[^"'"'"']*["'"'"'][[:space:]]*\.\.[[:space:]]*["'"'"']'; then
    errors="${errors}WARNING: Avoid string concatenation for paths - use path.join() instead\n"
fi

# Check for package.path manipulation
if echo "$content" | grep -q 'package\.path'; then
    errors="${errors}ERROR: Never manipulate package.path\n"
fi

# Check for octal literals (lua doesn't support them)
if echo "$content" | grep -qE '[^0-9]0[0-7]{3}[^0-9]'; then
    errors="${errors}ERROR: Lua has no octal notation - use tonumber(\"644\", 8) for permissions\n"
fi

if [[ -n "$errors" ]]; then
    echo -e "$errors" >&2
    exit 2  # Block
fi

exit 0
