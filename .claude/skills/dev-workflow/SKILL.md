---
name: dev-workflow
description: Development workflow for this repository. Run `make help` to see available targets.
allowed-tools: [Bash, Read, Write, Edit]
---

# Development workflow

This project uses a Makefile-based build system.

## Getting started

Run `make help` to see all available targets and understand the build workflow.

```bash
make help
```

## Pull request descriptions

**CRITICAL: You MUST create and maintain a PR description file for every branch.**

This file describes what's happening on the branch. Update it before every push.

1. Create `.github/pr/<slug>.md` with a descriptive slug (e.g., `add-auth-logging.md`, `fix-parser-edge-case.md`)
2. Use this format:

```markdown
# component: verb explanation

Brief description of what this change does and why.

## Changes

- `path/to/file.lua` - what it does
- `path/to/other.lua` - what it does
```

**Guidelines:**
- Choose a descriptive, kebab-case slug
- Title format: `component: verb explanation` (sentence case)
- Keep descriptions concise but include key decisions and tradeoffs
- **Update the file before every push** to reflect the current state of the branch
