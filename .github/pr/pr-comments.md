# skill: add pr-comments script to fetch github pr review comments

Fetches all comment types on a GitHub PR using cosmo.Fetch. Useful for reviewing feedback programmatically or integrating with other tools.

Changes:
- lib/skill/pr_comments.lua - fetches reviews, issue comments, and line-level review comments
- lib/skill/test_pr_comments.lua - unit tests for formatting and URL parsing
- lib/skill/init.lua - registers pr-comments as a skill subcommand

Features:
- **Multiple comment types** - reviews, issue comments, line-level review comments
- **Flexible input** - accepts owner/repo/number or full GitHub URL
- **Output formats** - markdown (default) or JSON
- **Retry logic** - exponential backoff for transient API errors (504s, etc)

Usage:
```bash
# markdown output
cosmic -l skill pr-comments whilp world 283
cosmic -l skill pr-comments https://github.com/whilp/world/pull/283

# json output
cosmic -l skill pr-comments whilp world 283 json
OUTPUT=json cosmic -l skill pr-comments whilp world 283
```

## Validation

- [x] tests pass
- [x] markdown output works
- [x] json output works
- [x] url parsing works
- [x] retry logic handles transient errors
