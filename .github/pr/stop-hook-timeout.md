# skill/hook: add stop_hook_active check and 15s timeout

Prevent infinite loops in stop hooks and add timeout safety.

- lib/skill/hook.lua - check stop_hook_active in Stop handlers to exit early when already continuing
- .claude/settings.json - add 15s timeout to all hooks (SessionStart, PostToolUse, Stop)

## Validation

- [x] tests pass
