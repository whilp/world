# skill: simplify PR lookup using git trailers

Use `x-cosmic-pr-name` git trailer to identify PR files instead of PR number lookup via GitHub API.

## Changes

- **lib/skill/pr.lua**:
  - Add `get_pr_name_from_trailer()` to extract trailer
    - Scans last 20 commits (HEAD~20..HEAD) for trailers
    - Takes the most recent (last) trailer found
    - Works with merge commits (scans all commits in range)
  - Add `is_pr_updates_enabled()` to check for opt-out
    - Returns false if `x-cosmic-pr-enable: false` trailer found
  - Add `get_commit_sha()` for debug output
  - Update `main()` to require trailer and use `GITHUB_PR_NUMBER` from env
  - Update `do_update()` to accept `pr_name` parameter
  - Remove API lookup functions: `find_pr_number`, `find_pr_for_branch`, `get_pr_number_from_env`, `get_git_info` (~140 lines)
  - Update help text to document trailer workflow
  - Improve error messages: show all 20 commits checked and their trailers

- **lib/skill/test_pr_*.lua**: Update tests for trailer-based approach

## Workflow

1. Create `.github/pr/<descriptive-name>.md`
2. Add trailer to commit: `x-cosmic-pr-name: <descriptive-name>.md`
3. Push - GitHub Actions reads trailer to find PR file

**Rename PR file:** Add new commit with different trailer:
```
x-cosmic-pr-name: new-name.md
```

**Disable PR updates:** Add trailer to any commit:
```
x-cosmic-pr-enable: false
```

## Benefits

- **No API calls** - scans git log locally
- **Deterministic filename** - choose before PR exists
- **Intuitive names** - `feature-auth.md` instead of `239.md`
- **Renaming support** - add new trailer in later commit
- **Opt-out option** - disable via `x-cosmic-pr-enable: false`
- **Simpler code** - removed ~140 lines of lookup logic
- **Better debugging** - shows all commits and trailers checked

## Validation

- [x] All tests pass
- [x] Help text updated
- [x] Handles merge commits (scans commit range)
- [x] Supports renaming (takes most recent trailer)
- [x] Supports disable (x-cosmic-pr-enable: false)
- [x] Debug output shows all commits and trailers
