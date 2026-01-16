# claude: add PATH-based binary search

Add `search_path_for_claude()` to find the claude binary in PATH directories, and add `/.sprite/bin/claude` as a known location.

## Changes

- `lib/claude/main.tl` - add `search_path_for_claude()` function that iterates PATH directories (excluding the wrapper itself), add `/.sprite/bin/claude` to search paths
