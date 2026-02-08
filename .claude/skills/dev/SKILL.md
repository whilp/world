---
name: dev
description: Development workflow for this repository. Run `make help` to see available targets.
---

# dev

Development workflow utilities for branching, PR management, and review feedback.

## Creating branches

Create a new branch off the latest default branch commit.

### Workflow

1. Fetch latest from remote
2. Detect default branch (main or master)
3. Create branch with descriptive name based on task
4. Switch to new branch

### Branch naming

Generate a short, kebab-case name from the task:
- `add-user-auth` not `add-user-authentication-feature`
- `fix-login-bug` not `fix-the-bug-in-the-login-flow`
- `refactor-api` not `refactor-api-endpoints-for-better-performance`

Use a prefix if the repo convention requires one (check existing branches).

```bash
# Fetch and get default branch
git fetch origin
git remote show origin | sed -n '/HEAD branch/s/.*: //p'

# Create branch from latest default branch
git checkout -b <branch-name> origin/main

# See existing branch naming patterns
git branch -r | head -20
```

If branches use prefixes like `feature/`, `fix/`, or username prefixes like `wcm/`, follow that pattern.

## Review feedback

Address GitHub PR review comments and reply to reviewers.

### Workflow

1. **Get pending review comments** - fetch unresolved comments
2. **Address each comment** - make code changes to resolve feedback
3. **Commit with clear message** - reference what feedback was addressed
4. **Reply to reviewer** - concise reply with commit SHA and explanation

### Getting review comments

```bash
# All comments on a PR (includes replies)
gh pr view <pr-number> --comments --json comments

# Review comments (code-level feedback)
gh api repos/{owner}/{repo}/pulls/{pr}/comments --jq '.[] | {id, path, line, body, user: .user.login, in_reply_to_id}'

# Pending/unresolved comments (no replies yet)
gh api repos/{owner}/{repo}/pulls/{pr}/comments --jq '[.[] | select(.in_reply_to_id == null)] | .[] | {id, path, body: .body[0:100]}'
```

### Replying to comments

```bash
# Reply to a review comment
gh api repos/{owner}/{repo}/pulls/{pr}/comments \
  --method POST \
  -f body="Fixed in abc1234 - renamed to helper.js" \
  -F in_reply_to=<comment-id>
```

### Good reply format

Replies should be:
- **Concise** - one sentence is often enough
- **Reference commit SHA** - so reviewer can verify
- **Explain how** - brief description of the fix

Examples:
- `Fixed in abc1234 - renamed to helper.js as suggested`
- `Addressed in def5678 - now uses a single rule in cook.mk`
- `Good catch, fixed in 789abcd - removed the duplicate import`

### Complete example

```bash
# 1. Get pending comments on PR #269
gh api repos/whilp/world/pulls/269/comments \
  --jq '.[] | select(.in_reply_to_id == null) | {id, path, body}'

# Output:
# {"id":2702089939,"path":"lib/appscript/helpers.js","body":"helpers.js should be helper.js..."}

# 2. Make the fix and commit
git mv lib/appscript/helpers.js lib/appscript/helper.js
git commit -m "appscript: rename helpers.js to helper.js"
# Commit SHA: abc1234

# 3. Reply to the comment
gh api repos/whilp/world/pulls/269/comments \
  --method POST \
  -f body="Fixed in abc1234 - renamed to helper.js" \
  -F in_reply_to=2702089939
```

### Batch processing

For multiple comments:

```bash
# Get all pending comments as JSON
gh api repos/{owner}/{repo}/pulls/{pr}/comments \
  --jq '[.[] | select(.in_reply_to_id == null)] | group_by(.path) | .[] | {path: .[0].path, comments: [.[] | {id, body}]}'
```

This groups comments by file for efficient processing.
