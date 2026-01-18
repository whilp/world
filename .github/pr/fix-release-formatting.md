# build: fix release formatting

Remove redundant "home-" prefix from release tags and clean up release presentation.

Previously, releases appeared as "home home-2026-01-11-f977087" with escaped newlines in the description. Now they use clean date-based tags (YYYY-mm-dd-SHA) with no description.

## Changes

- Makefile:278 - Changed tag format from `home-YYYY-mm-dd-SHA` to `YYYY-mm-dd-SHA`
- Makefile:288 - Changed title from `--title "home $$tag"` to `--title "$$tag"`
- Makefile:289 - Removed `--notes` parameter entirely

## Validation

- [x] Tag format matches YYYY-mm-dd-SHA pattern
- [x] Title no longer duplicates "home" prefix
- [x] No description text in release
