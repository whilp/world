# skill/hook: convert stop hooks to post tool use on git push

Convert stop hooks to PostToolUse hooks triggered on git push instead of session end.

- settings.json: remove Stop hook, change PostToolUse matcher to "" (match all)
- hook.lua: stop_check_commit_trailer → post_push_pr_check
- hook.lua: stop_check_reminder → post_push_check_reminder
- test_hook.lua: update tests for new post_push handler behavior

## Rationale

Stop hooks fire at session end which is too late for actionable feedback. By triggering on git push, users get immediate feedback about PR descriptions and running checks right when they push.

## Validation

- [x] make test passes
- [x] hook tests updated and passing
