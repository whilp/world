# skill: simplify PR lookup using git trailers

Use `x-cosmic-pr-name` git trailer to identify PR files instead of PR number lookup via GitHub API.

## Changes

- **lib/skill/pr.lua**:
  - Add `get_pr_name_from_trailer()` to extract trailer from HEAD commit
  - Update `main()` to require trailer and use `GITHUB_PR_NUMBER` from env
  - Update `do_update()` to accept `pr_name` parameter
  - Remove API lookup functions: `find_pr_number`, `find_pr_for_branch`, `get_pr_number_from_env`, `get_git_info`
  - Update help text to document trailer workflow

- **lib/skill/test_pr_*.lua**: Update tests for trailer-based approach

## Workflow

1. Create `.github/pr/<descriptive-name>.md`
2. Add trailer to commit: `x-cosmic-pr-name: <descriptive-name>.md`
3. Push - GitHub Actions reads trailer to find PR file

## Benefits

- **No API calls** in local mode - just read git trailer
- **Deterministic filename** before PR even exists
- **More intuitive** - choose meaningful names like `feature-auth.md` instead of `239.md`
- **Simpler code** - removed ~140 lines of lookup logic

## Validation

- [x] All tests pass
- [x] Help text updated
- [x] Example PR file created
