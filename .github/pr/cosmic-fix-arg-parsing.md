# cosmic: fix arg parsing to stop at script name

Fixes argument parsing bug where script arguments were being consumed by cosmic's option parser instead of being passed to the script.

## Problem

When running a lua script via cosmic-lua, arguments like `--version` after the script name were consumed by cosmic-lua's option parser instead of being passed to the script:

```bash
# Expected: script sees arg[1] = "--version"
# Actual: cosmic-lua consumes --version, script sees no args
cosmic script.lua --version
```

This broke wrapper scripts like nvim that depend on receiving their own command-line flags.

## Root cause

`parse_args()` was passing all arguments to getopt, which would parse unknown options (like `--version` after the script name) and consume them, rather than stopping at the script name boundary.

## Solution

Implemented pre-scan approach that:
- Scans arguments to find the first non-option argument (the script name)
- Handles `--` separator as an explicit marker for end-of-options
- Only passes arguments before the script name to getopt for parsing
- Preserves all arguments after the script name as script arguments
- Properly excludes `--` separators from script args

This matches standard interpreter behavior (python, ruby, etc.) where option parsing stops at the script boundary.

## Changes

- lib/cosmic/main.tl - refactor parse_args() to pre-scan for script name
  - Pre-scan arg array to locate script name before calling getopt
  - Only pass cosmic options (before script name) to getopt
  - Handle `--` separator to explicitly mark end of cosmic options
  - Handle `?` return from getopt as genuine unknown option error
  - Skip `--` separator when building script_args array

- lib/cosmic/test_args.tl - add comprehensive tests for arg parsing fix
  - Test --version flag passed to script (not consumed by cosmic -v)
  - Test --help flag passed to script (not consumed by cosmic --help)
  - Test -v flag passed to script after script name
  - Test mixed cosmic options before script and script options after
  - Test multiple option-like arguments after script name
  - Test -- separator without following options
  - Test real-world nvim-like pattern (script.lua --version)

## Validation

- [x] all existing tests pass
- [x] new tests validate --version, --help, -v after script name
- [x] cosmic's own options still work correctly (cosmic -v, cosmic -e, etc.)
- [x] -- separator works as expected
- [x] mixed cosmic and script options parse correctly
