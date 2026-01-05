# build: chmod test files before execution

Ensures test files are executable before running them, handling cases where files may lose executable permissions (e.g., after certain git operations or archive extraction).

## Implementation

- Makefile:135 - Added `@[ -x $< ] || chmod a+x $<` before test execution

## Design

The solution is minimal and idempotent:
- Only chmod when needed (`[ -x $< ]` check prevents unnecessary operations)
- Operates on test file only, not entire directories
- Preserves mtimes and make's incremental build performance
- Simple one-liner vs complex recursive find operations

Initial approach used `chmod -R +x` on staged directories, but this was overkill - making README files executable, potentially touching thousands of files (nvim has 2104 files), and less idempotent.

## Validation

- [x] Test files without +x are automatically fixed
- [x] Already executable files are not touched (idempotent)
- [x] All tests pass (29 passed, 2 skipped)
- [x] Incremental builds remain instant (0.2s)
