# skill: simplify pr error message and exit 0

Revises the error handling when no x-cosmic-pr-name trailer is found in the commit history.

- lib/skill/pr.lua - returns exit code 0 instead of 1 when no trailer found
- lib/skill/pr.lua - shows commit range (first..last) instead of just HEAD
- lib/skill/pr.lua - removes verbose debug sections (commits/trailers listing)
- lib/skill/pr.lua - keeps only essential message and note

## Changes

The error message now shows:
```
no x-cosmic-pr-name trailer found (or disabled) in last 20 commits (<first_sha> .. <last_sha>)

Note: x-cosmic-pr-enable: false disables updates until a new x-cosmic-pr-name is set
```

Instead of the previous verbose output with full commit and trailer listings.

## Validation

- [x] tests pass
- [x] exit code changed from 1 to 0
- [x] message format simplified
