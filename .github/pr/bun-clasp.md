# build: add bun and clasp to releases

Add bun runtime and Google Apps Script CLI (clasp) to the release artifacts.

- 3p/bun/version.lua - bun v1.3.6 binary for all platforms
- 3p/bun/cook.mk - module definition
- 3p/bun/test_bun.tl - verifies bun binary works
- 3p/clasp/version.lua - fetches clasp v3.1.3 source from GitHub
- 3p/clasp/cook.mk - builds clasp using `bun install` + `bun build --compile`
- 3p/clasp/test_clasp.tl - tests clasp CLI
- Makefile - includes bun/clasp modules, adds clasp to release
- lib/home/cook.mk - bundles clasp in home asset
- .github/workflows/release.yml - uploads clasp artifact for release

## Validation

- [x] bun tests pass
- [x] clasp tests pass
