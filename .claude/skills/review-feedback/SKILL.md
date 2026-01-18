---
name: review-feedback
description: Address GitHub PR review comments and reply to reviewers. Use when handling review feedback, responding to code review comments, or checking pending review items on a pull request.
---

# review-feedback

Address GitHub PR review comments and reply to reviewers.

## Workflow

1. **Get pending review comments** - fetch unresolved comments
2. **Address each comment** - make code changes to resolve feedback
3. **Commit with clear message** - reference what feedback was addressed
4. **Reply to reviewer** - concise reply with commit SHA and explanation

## Getting review comments

```bash
# All comments on a PR (includes replies)
gh pr view <pr-number> --comments --json comments

# Review comments (code-level feedback)
gh api repos/{owner}/{repo}/pulls/{pr}/comments --jq '.[] | {id, path, line, body, user: .user.login, in_reply_to_id}'

# Comments from a specific review
gh api repos/{owner}/{repo}/pulls/{pr}/reviews/{review-id}/comments --jq '.[] | {id, path, body}'

# Pending/unresolved comments (no replies yet)
gh api repos/{owner}/{repo}/pulls/{pr}/comments --jq '[.[] | select(.in_reply_to_id == null)] | .[] | {id, path, body: .body[0:100]}'
```

## Addressing feedback

For each comment:
1. Read the feedback carefully
2. Make the requested change (or explain why not)
3. Commit with a descriptive message
4. Reply referencing the commit

## Replying to comments

```bash
# Reply to a review comment
gh api repos/{owner}/{repo}/pulls/{pr}/comments \
  --method POST \
  -f body="Fixed in abc1234 - renamed to run-test.js" \
  -F in_reply_to=<comment-id>
```

## Good reply format

Replies should be:
- **Concise** - one sentence is often enough
- **Reference commit SHA** - so reviewer can verify
- **Explain how** - brief description of the fix

Examples:
- `Fixed in abc1234 - renamed to run-test.js as suggested`
- `Addressed in def5678 - now uses a single rule in cook.mk`
- `Good catch, fixed in 789abcd - removed the duplicate import`

## Complete example

```bash
# 1. Get pending comments on PR #269
gh api repos/whilp/world/pulls/269/comments \
  --jq '.[] | select(.in_reply_to_id == null) | {id, path, body}'

# Output:
# {"id":2702089939,"path":"lib/appscript/run-tests.js","body":"run-tests.js should be run-test.js..."}

# 2. Make the fix and commit
git mv lib/appscript/run-tests.js lib/appscript/run-test.js
git commit -m "appscript: rename run-tests.js to run-test.js"
# Commit SHA: abc1234

# 3. Reply to the comment
gh api repos/whilp/world/pulls/269/comments \
  --method POST \
  -f body="Fixed in abc1234 - renamed to run-test.js" \
  -F in_reply_to=2702089939
```

## Batch processing

For multiple comments:

```bash
# Get all pending comments as JSON
gh api repos/{owner}/{repo}/pulls/{pr}/comments \
  --jq '[.[] | select(.in_reply_to_id == null)] | group_by(.path) | .[] | {path: .[0].path, comments: [.[] | {id, body}]}'
```

This groups comments by file for efficient processing.
