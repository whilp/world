# skill/hook: add claude code hook dispatcher library

Adds a centralized hook dispatcher for Claude Code hooks. Instead of separate scripts for each hook type, this provides a single entry point that receives JSON via stdin and dispatches to registered handlers.

Changes:
- lib/skill/hook/init.lua - hook dispatcher with registration system and built-in SessionStart handler
- lib/skill/hook/test_hook.lua - comprehensive tests for parsing, dispatch, and bootstrap behavior
- lib/skill/cook.mk - include hook subdirectory in skill module
- lib/cosmic/cook.mk - bundle hook into cosmic binary
- bin/hook - wrapper script for development (runs from lib/ source)
- .claude/settings.json - use bin/hook for SessionStart

Features:
- `hook.register(event, handler)` - register handlers for any hook event type
- `hook.read_input()` - parse JSON from stdin
- `hook.dispatch(input)` - route to registered handlers, merge outputs
- `hook.run()` - main entry point for skill invocation
- Built-in SessionStart handler appends bin/ to PATH via CLAUDE_ENV_FILE

The library is extensible - register custom handlers for PreToolUse, PostToolUse, Stop, etc. Development uses bin/hook which loads from lib/ via LUA_PATH for fast iteration without rebuilding cosmic.

## Validation

- [x] tests pass (lib/skill/hook/test_hook.lua)
- [x] `cosmic --skill hook` works with compiled binary
- [x] `bin/hook` works with source files
- [x] SessionStart bootstrap appends PATH correctly
