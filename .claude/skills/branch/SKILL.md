---
name: branch
description: Create a new git branch for a task. Use when starting new work, creating feature branches, or when user says /branch with a task description.
user-invocable: true
---

# branch

Create a new branch off the latest default branch commit.

## Workflow

1. Fetch latest from remote
2. Detect default branch (main or master)
3. Create branch with descriptive name based on task
4. Switch to new branch

## Branch naming

Generate a short, kebab-case name from the task:
- `add-user-auth` not `add-user-authentication-feature`
- `fix-login-bug` not `fix-the-bug-in-the-login-flow`
- `refactor-api` not `refactor-api-endpoints-for-better-performance`

Use a prefix if the repo convention requires one (check existing branches).

## Commands

```bash
# Fetch and get default branch
git fetch origin
git remote show origin | sed -n '/HEAD branch/s/.*: //p'

# Create branch from latest default branch
git checkout -b <branch-name> origin/main

# Or if using master
git checkout -b <branch-name> origin/master
```

## Example

Task: "add dark mode toggle to settings"

```bash
git fetch origin
git checkout -b add-dark-mode-toggle origin/main
```

## Checking repo conventions

```bash
# See existing branch naming patterns
git branch -r | head -20
```

If branches use prefixes like `feature/`, `fix/`, or username prefixes like `wcm/`, follow that pattern.
