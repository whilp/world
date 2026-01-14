# claude: add PR description template instructions

Adds a critical instruction to `.claude/CLAUDE.md` requiring PR description files for every branch.

## Changes

- `.claude/CLAUDE.md` - added "Pull request descriptions" section requiring:
  - Creation of `.github/pr/<slug>.md` for every branch
  - Keeping the file up to date before every push
  - Format template with title, changes, and validation sections
  - Commit trailer usage (`x-cosmic-pr-name`)

## Why

Ensures all branches have proper documentation describing what's happening, which the `cosmic --skill pr` workflow uses to automatically update GitHub PR titles and descriptions.

## Validation

- [x] instructions match existing PR file format
- [x] compatible with `cosmic --skill pr`
