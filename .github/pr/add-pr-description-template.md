# claude: add PR description template instructions

Adds a critical instruction to `.claude/CLAUDE.md` requiring PR description files for every branch.

## Changes

- `.claude/CLAUDE.md` - added "Pull request descriptions" section requiring:
  - Creation of `.github/pr/<slug>.md` for every branch
  - Keeping the file up to date before every push
  - Format template with title and changes sections
