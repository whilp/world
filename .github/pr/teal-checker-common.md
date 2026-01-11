## Summary

- Convert `lib/checker/common.lua` to `lib/checker/common.tl` with full type annotations (PR 2.1 from teal migration plan)
- Add build system support for `.tl` files (PR 1.2 from teal migration plan)
- Add requirement for ast-grep support on `.tl` files (PR 1.3)

## Changes

**Type annotations:**
- Define record types: `CheckResult`, `CheckPatterns`, `CategorizedResults`, `StatusIcons`
- Add type annotations to all functions in checker/common

**Build system:**
- Update `lib/checker/cook.mk` to compile `.tl` files to `o/teal/lib/`
- Add `o/teal/lib` to `LUA_PATH` in Makefile for compiled module resolution
- Add `checker_files` and `tl_staged` dependencies to linter rules
- Add pattern rule `o/teal/lib/%.lua: lib/%.tl` for compilation via `tl gen -o`
- Use `.SECONDEXPANSION` for deferred `$(tl_staged)` evaluation (fixes parallel builds)

## Test plan

- [x] `make teal` passes (29 passed, 0 failed)
- [x] `make test` passes for checker tests
- [x] `make check` passes for all linters
- [x] `make -j4 ci` passes (parallel build)
