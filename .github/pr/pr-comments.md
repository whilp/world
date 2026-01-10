# skill: add pr-comments script to fetch github pr review comments

Fetches all comment types on a GitHub PR using cosmo.Fetch. Useful for reviewing feedback programmatically or integrating with other tools.

Changes:
- lib/skill/pr_comments.lua - fetches reviews, issue comments, and line-level review comments
- lib/skill/test_pr_comments.lua - unit tests for formatting and URL parsing
- lib/skill/init.lua - simplified to just export version info

Features:
- **Multiple comment types** - reviews, issue comments, line-level review comments
- **Flexible input** - accepts --owner/--repo/--pr or --url
- **Output formats** - markdown (default) or JSON
- **Retry logic** - exponential backoff for transient API errors (504s, etc)

Usage:
```bash
# by owner/repo/pr
cosmic --skill pr_comments --owner whilp --repo world --pr 283

# by url
cosmic --skill pr_comments --url https://github.com/whilp/world/pull/283

# json output
cosmic --skill pr_comments -o whilp -r world -p 283 --json
```

## Validation

- [x] tests pass
- [x] markdown output works
- [x] json output works
- [x] url parsing works
- [x] retry logic handles transient errors
