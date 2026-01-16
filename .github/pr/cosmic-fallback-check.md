# skill/pr: use GITHUB_BASE_REF for shallow clone compatibility

The pr skill's file-based fallback detection (`git diff origin/main...HEAD`)
fails in CI with shallow clones because `origin/main` isn't fetched when
checking out the PR head with limited depth.

## Changes

- `lib/skill/pr.tl` - simplify `get_default_branch()` to use `GITHUB_BASE_REF`
- `.github/workflows/pr.yml` - fetch base branch, run from source until next release
- `lib/skill/test_pr.tl` - simplify tests to match new implementation

## Implementation

Since the pr skill only runs in GitHub Actions, we can simplify `get_default_branch()`
to just use `GITHUB_BASE_REF` (automatically set to the PR target branch).

The workflow now explicitly fetches the base branch with `--depth=1` before
running the pr skill, ensuring the merge base is available for the git diff.

## Follow-up

After merging and cutting a new release, revert the workflow to use
`--skill pr` instead of `-e 'require("lib.skill.pr").main()'`.
