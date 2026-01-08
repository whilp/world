# make: fix jobserver warning from forced -j flag

Fixes the `-j4 forced in makefile: resetting jobserver mode` warning in CI.

**Changes:**
- Makefile - remove forced `-j` from MAKEFLAGS
- .github/workflows/pr.yml - add `-j$(nproc)` to make invocation
- .github/workflows/release.yml - add `-j$(nproc)` to make invocations

**How it works:**
- Makefile no longer forces parallel jobs in MAKEFLAGS
- CI explicitly sets `-j$(nproc)` on command line
- Recursive makes inherit jobserver via file descriptors
- No jobserver mode conflicts

## Validation

- [x] no jobserver warnings
- [x] recursive makes coordinate properly
- [x] parallel builds work in CI
