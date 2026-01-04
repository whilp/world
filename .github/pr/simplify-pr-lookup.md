# skill: simplify PR lookup using git trailers

Use `x-cosmic-pr-name` git trailer to identify PR files instead of PR number lookup via GitHub API.

## Changes

- **lib/skill/pr.lua**:
  - Add `get_pr_name_from_trailer()` to extract trailer from HEAD commit
    - Checks HEAD first for trailer
    - If HEAD is merge commit (GitHub Actions), checks HEAD^1 (PR branch)
  - Add `get_commit_sha()` for debug output
  - Update `main()` to require trailer and use `GITHUB_PR_NUMBER` from env
  - Update `do_update()` to accept `pr_name` parameter
  - Remove API lookup functions: `find_pr_number`, `find_pr_for_branch`, `get_pr_number_from_env`, `get_git_info` (~140 lines)
  - Update help text to document trailer workflow
  - Improve error messages with commit SHA, message, and trailers for debugging

- **lib/skill/test_pr_*.lua**: Update tests for trailer-based approach

## Workflow

1. Create `.github/pr/<descriptive-name>.md`
2. Add trailer to commit: `x-cosmic-pr-name: <descriptive-name>.md`
3. Push - GitHub Actions reads trailer to find PR file

## GitHub Actions Handling

GitHub Actions checks out a merge commit (PR merged into base), not the actual PR commit. The merge commit doesn't have the trailer, so we check the first parent (HEAD^1) which is the PR's latest commit.

## Benefits

- **No API calls** in local mode - just read git trailer
- **Deterministic filename** before PR even exists
- **More intuitive** - choose meaningful names like `feature-auth.md` instead of `239.md`
- **Simpler code** - removed ~140 lines of lookup logic
- **Better debugging** - error shows commit SHA and all trailers

## Validation

- [x] All tests pass
- [x] Help text updated
- [x] Handles merge commits (GitHub Actions)
- [x] Debug output shows commit details
