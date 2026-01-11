# skill/pr: auto-detect PR file when branch introduces exactly one

When no `x-cosmic-pr-name` trailer is specified, the PR skill now automatically detects if the branch introduces exactly one `.github/pr/*.md` file and uses it. This reduces friction for simple PRs where there's only one PR file.

## Changes

- `lib/skill/pr.lua` - added `get_pr_files_from_branch()` to find PR files introduced by the branch, integrated fallback logic in `main()`
- `lib/skill/test_pr.lua` - added tests for branch file detection and main() fallback behavior
