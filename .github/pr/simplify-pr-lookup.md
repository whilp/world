# skill: simplify PR lookup using git trailers

Use `x-cosmic-pr-name` git trailer to identify PR files instead of PR number lookup via GitHub API.

## Changes

- **lib/skill/pr.lua**:
  - Combine `get_pr_name_from_trailer()` and `is_pr_updates_enabled()` into single function
  - Scan last 20 commits for trailers, process chronologically
  - `x-cosmic-pr-enable: false` disables until new `x-cosmic-pr-name` is set
  - Remove ~140 lines of API lookup code
  - Improve error messages with debug info

- **lib/skill/test_pr.lua**: Consolidated tests using isolated git repos

## Workflow

1. Create `.github/pr/YYYY-MM-DD-<slug>.md` (e.g., `2026-01-04-add-auth.md`)
2. Add trailer to commit: `x-cosmic-pr-name: 2026-01-04-add-auth.md`
3. Push - GitHub Actions reads trailer to find PR file

**Rename PR file:** Add new trailer in later commit.

**Disable PR updates:** Add `x-cosmic-pr-enable: false` trailer.

## Workflow configuration

```yaml
update:
  if: github.event_name == 'pull_request'
  runs-on: ubuntu-latest
  steps:
    - uses: actions/checkout@v4
      with:
        ref: ${{ github.event.pull_request.head.sha }}
        fetch-depth: 20

    - run: make update-pr
      env:
        GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
        GITHUB_PR_NUMBER: ${{ github.event.number }}
```

The `ref` and `fetch-depth` settings are required - default checkout uses a shallow merge commit without access to branch commits with trailers.

## Benefits

- **No API calls** - scans git log locally
- **Deterministic filename** - choose before PR exists
- **Intuitive names** - date prefix keeps PRs organized
- **Renaming support** - add new trailer in later commit
- **Opt-out option** - disable via `x-cosmic-pr-enable: false`

## Validation

- [x] All tests pass
- [x] Workflow checkout configured correctly
- [x] Supports trailer scanning and disable
