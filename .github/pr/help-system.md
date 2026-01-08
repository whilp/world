# make: add self-documenting help system

Implements a self-documenting Makefile help system with `##` comment syntax and snapshot testing infrastructure.

## Changes

- **Makefile** - Added `.DEFAULT_GOAL := help`, documented all main targets with `##` comments, added snapshot testing pattern rule
- **lib/build/make-help.lua** - Parser and formatter for extracting `##` comments from Makefiles, supports multiline comments and variable documentation
- **lib/build/test-snap.lua** - Generalized snapshot testing script using diff comparison
- **lib/build/test_help.lua** - Unit tests for help parser functions
- **lib/build/make-help.snap** - Expected help output snapshot for testing
- **lib/build/cook.mk** - Added `build_snaps` and rule to generate help snapshot

## Key design decisions

- Used multiline `##` comments only (no inline comments) for cleaner parsing
- Built as reusable Lua module that exports `parse_makefile` and `format_help` for testing
- Snapshot testing pattern generalized to Makefile so any module can add snapshots
- Comparison logic in Lua (test-snap.lua) rather than inline Make/bash
- Supports `filter-only` for `make test only=snap`

## Validation

- [x] `make help` shows all documented targets and options
- [x] `make test only=help` passes (unit tests + snapshot)
- [x] `make test only=snap` works with filter-only
- [x] Snapshot mismatch detection works correctly
- [x] All tests pass
