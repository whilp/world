# skill/hook: add claude code hook dispatcher library

Centralized hook dispatcher for Claude Code hooks. Single entry point receives JSON via stdin and dispatches to registered handlers.

Changes:
- lib/skill/hook.lua - hook dispatcher with registration system
- lib/skill/test_hook.lua - tests for parsing, dispatch, and handlers
- bin/hook - wrapper for development (loads from lib/ via LUA_PATH)
- .claude/settings.json - dispatch SessionStart, PostToolUse, Stop to bin/hook

Built-in handlers:
- **session_start_bootstrap** - appends bin/ to PATH via CLAUDE_ENV_FILE
- **session_start_make_help** - outputs `make help` on startup
- **post_commit_pr_reminder** - reminds to maintain .github/pr/<name>.md after commits
- **stop_check_pr_file** - blocks stop if PR file missing or stale

API:
- `hook.register(handler)` - register handler, receives input, returns output or nil
- `hook.dispatch(input)` - call all handlers, merge outputs
- `hook.run()` - read stdin, dispatch, write stdout

Handlers self-select by checking `input.hook_event_name`. This allows handlers to respond to multiple event types or apply cross-cutting logic.

## Validation

- [x] tests pass
- [x] `bin/hook` works with source files
- [x] SessionStart outputs make help
- [x] PostToolUse reminds about PR file after commits
- [x] Stop blocks if PR file is stale
