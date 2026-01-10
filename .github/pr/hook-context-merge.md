# skill/hook: merge additionalContext from multiple handlers

Fix dispatch() to concatenate additionalContext values when multiple PostToolUse
handlers return them, instead of last-wins overwrite.

- lib/skill/hook.lua - collect contexts array and join with newlines
- lib/skill/test_hook.lua - add test_dispatch_concatenates_additional_context

## Validation

- [x] make test only=hook passes
- [x] manual test: git push shows both PR hint and checks reminder
