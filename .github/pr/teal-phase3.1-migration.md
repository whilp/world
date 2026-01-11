# lib: migrate phase 3.1 standalone modules to teal

Migrate 3 small standalone library modules to Teal using parallel agents, completing Batch 3.1 from the teal migration plan.

## Changes

- `lib/environ/init.tl` - environment variable handling with `Environ` record type
- `lib/daemonize/init.tl` - process daemonization with typed unix operations
- `lib/whereami/init.tl` - location detection with union types for cached state

## Type declarations updated

- `lib/types/cosmo/unix.d.tl` - extended with dup2 and fcntl signatures for daemonize

## Migration approach

Used 3 parallel agents to migrate files concurrently:
- Each agent read the source file and reference `.tl` files for patterns
- Created type annotations following established conventions
- Updated `cook.mk` to compile from `.tl` instead of copying `.lua`

## Validation

- [x] `make teal` passes
- [x] `make test` passes (37 tests, 36 passed, 1 skipped)
