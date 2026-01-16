# test: convert remaining luaunit tests to assert style

Four test files still used luaunit which was removed in a9b02229.
Convert them to simple assert-based tests.

## Changes

- `lib/claude/test_claude.tl` - convert to assert style
- `lib/daemonize/test_daemonize.tl` - convert to assert style
- `lib/nvim/test_nvim.tl` - convert to assert style, remove tests for unexported functions
- `lib/whereami/test_whereami.tl` - convert to assert style
