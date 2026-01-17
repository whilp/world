# skill/pr: simplify to file-based detection only

Remove trailer-based PR file detection (`x-cosmic-pr-name`) in favor of
simple file detection: if the branch adds exactly one `.github/pr/*.md`
file, use it.

## Changes

- `lib/skill/pr.tl` - remove trailer logic, simplify to file detection
- `lib/skill/test_pr.tl` - remove trailer tests, simplify test suite

## Why

The trailer approach was complex and error-prone. Merged commits containing
trailers would pollute the detection logic, causing the wrong PR file to
be selected (as seen in PR #381 where `cosmic-fallback-check.md` was
picked instead of the correct file).

The file-based approach is simpler: one PR file added = use it.
