## Summary

- Add ast-grep support for `.tl` files (PR 1.3 from teal migration plan)
- Enable linting of teal files using lua parser

## Changes

- `3p/ast-grep/run-astgrep.lua` - add `.tl` to `supported_extensions`
- `sgconfig.yml` - add `**/*.tl` to `languageGlobs.lua`
- `docs/teal-migration.md` - mark PR 1.3 as done

## Test plan

- [x] `make astgrep` passes (134 checks, 125 passed)
- [x] `lib/checker/common.tl` is linted successfully
- [x] `make test` passes
