# skill/pr: add .md extension when trailer omits it

When the `x-cosmic-pr-name` trailer value doesn't include the `.md` extension, the pr skill now appends it automatically. This allows users to write either:

```
x-cosmic-pr-name: feature-name
x-cosmic-pr-name: feature-name.md
```

Both will correctly resolve to `.github/pr/feature-name.md`.

## Changes

- `lib/skill/pr.lua` - added `normalize_pr_name()` helper that appends `.md` if missing
- `lib/skill/test_pr.lua` - added test verifying extension-less trailers work
