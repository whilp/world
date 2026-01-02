# build: add pr.lua script to update PR title and description

Add a GitHub Action workflow that updates PR titles and descriptions from a `.github/pr.md` file.

- `lib/build/pr.lua` - parses pr.md and updates PR via GitHub API
- `.github/workflows/pr-update.yml` - workflow triggered on PR events
- `lib/build/cook.mk` - adds `make update-pr` target
- `lib/build/test_pr.lua` - tests for parsing and API functions

Uses dependency injection for fetch to enable testing without network calls.
Always exits 0 to never block builds.

## Validation

- [x] `make luatest` passes (15 tests)
- [x] `make luacheck` passes
- [x] Tested in CI via this PR
