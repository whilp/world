# Add GitHub Action to update PR title and description from file

This PR adds a workflow that automatically updates PR titles and descriptions based on a `.github/pr.md` file.

## Changes

- `lib/build/pr.lua` - script to parse pr.md and update PR via GitHub API
- `.github/workflows/pr-update.yml` - workflow triggered on PR events
- `lib/build/cook.mk` - adds `make update-pr` target

## Features

- Parses markdown format: `# title` followed by description body
- Uses dependency injection for testability (mock fetch in tests)
- Always exits 0 to never block builds
- Only runs when `.github/pr.md` exists
