# build: add bun and clasp to home

Add bun runtime and Google Apps Script CLI (clasp) to the home binary.

- bun enables JavaScript/TypeScript tooling and package management
- clasp enables build-time tests and runtime interaction with Apps Script projects

## Changes

- `3p/bun/version.lua` - bun v1.3.6 binary for all platforms
- `3p/bun/cook.mk` - module definition
- `3p/bun/test_bun.tl` - verifies bun binary works
- `3p/clasp/version.lua` - fetches clasp v3.1.3 source from GitHub
- `3p/clasp/cook.mk` - builds clasp using `bun install` + `bun build --compile`
- `3p/clasp/bun.lock` - lockfile for reproducible builds
- `3p/clasp/test_clasp.tl` - tests clasp CLI
- `Makefile` - includes bun/clasp modules
- `lib/home/cook.mk` - bundles bun and clasp in home

## Validation

- [x] bun tests pass
- [x] clasp tests pass
