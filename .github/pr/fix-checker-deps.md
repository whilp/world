## Summary

- Add missing `$(checker_files)` dependency to update and test rules in Makefile
- Fixes `make update` failing after checker.common migration to teal

## Changes

- `Makefile:251` - Add `$(checker_files)` to update rule prerequisites
- `Makefile:158` - Add explicit `$(checker_files)` to test rule prerequisites

The update rule was missing the dependency on `$(checker_files)`, causing `check-update.lua` to fail with "module 'checker.common' not found" since the teal file wasn't compiled before running the script.

The test rule had implicit coverage via auto-expansion (lines 169-180), but making it explicit matches the pattern used by ast-grep, luacheck, and teal rules.

## Test plan

- [x] `make clean && make update` succeeds
- [x] `make clean && make test only=checker` succeeds
- [x] `make clean && make check only=checker` succeeds
