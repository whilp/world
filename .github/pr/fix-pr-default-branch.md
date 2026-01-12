# skill/pr: fix default branch detection when origin/HEAD not set

The pr skill's branch file auto-detection was failing when origin/HEAD
wasn't configured, which is common in repos that don't run
`git remote set-head origin --auto`.

## Changes

- lib/skill/pr.tl:108 - Added `get_default_branch()` function with smart fallback logic
- lib/skill/pr.tl:157 - Updated `get_pr_files_from_branch()` to use `get_default_branch()`
- lib/skill/test_pr.tl:466 - Added 4 comprehensive tests for all fallback scenarios

## Implementation

The new `get_default_branch()` function tries multiple strategies in order:

1. Run `git remote show origin` to find the HEAD branch
2. Check if `origin/main` exists using `git rev-parse --verify`
3. Check if `origin/master` exists
4. Fall back to `origin/HEAD` (original behavior)

This ensures the pr skill works in repos with non-standard setups while
maintaining backward compatibility.

## Validation

- [x] All existing tests pass
- [x] Added tests for all fallback paths
- [x] Verified `bin/cosmic --skill pr` works correctly
- [x] Tested with simulated GitHub Actions environment
