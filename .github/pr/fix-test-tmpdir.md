# test: set TEST_TMPDIR as environment variable

Fixes tests creating temporary files in the repository root by setting TEST_TMPDIR as both a global and environment variable.

- lib/test/run-test.lua - Add unix.setenv() to export TEST_TMPDIR to environment
- lib/checker/test_common.lua - Access TEST_TMPDIR via os.getenv()
- lib/cosmic/test_args.lua - Access TEST_TMPDIR via os.getenv()

## Validation

- [x] All 32 tests pass (31 passed, 1 skipped, 0 failed)
- [x] No untracked files created in repository root
- [x] Verified with `make clean && make test`
