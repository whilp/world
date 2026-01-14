# claude: add PR description template instructions

Adds a critical instruction to `.claude/CLAUDE.md` requiring PR description files for every change.

## Changes

- `.claude/CLAUDE.md` - added "Pull request descriptions" section with format requirements, commit trailer usage, and guidelines

## Why

Ensures all changes have proper documentation in `.github/pr/<slug>.md` files, which the `cosmic --skill pr` workflow uses to automatically update GitHub PR titles and descriptions.

## Validation

- [x] instructions match existing PR file format
- [x] compatible with `cosmic --skill pr`
