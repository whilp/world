# lib: migrate ulid.lua to teal

Migrate lib/ulid.lua to Teal as part of the ongoing type migration effort (PR 2.2).

- lib/ulid.tl - ULID generation with DecodedUlid record type
- lib/cook.mk - add lib_srcs for standalone file checking, use -o flag for tl_gen
- 3p/tl/tl-gen.lua - use getopt for argument parsing
- docs/teal-migration.md - update migration status

## Validation

- [x] `make teal` passes (139 checks, 34 passed)
- [x] `make test` passes (36/37 tests)
- [x] `make clean test` verifies full rebuild
