# lib: migrate phase 2 core modules to teal

Migrate 4 core library files to Teal as part of PR 2.3 and PR 2.4 from the teal migration plan.

## Changes

- `lib/utils.tl` - utility functions with generic type annotations
- `lib/platform.tl` - platform detection with const maps
- `lib/file.tl` - file operations with posix type declarations
- `lib/cosmic/spawn.tl` - process spawning with Pipe/SpawnHandle records

## Type declarations added

- `lib/types/version.d.tl` - version module API
- `lib/types/posix/sys/stat.d.tl` - stat, S_ISDIR, S_ISREG
- `lib/types/posix/dirent.d.tl` - dir, files
- `lib/types/posix/unistd.d.tl` - readlink, rmdir, etc.

## Validation

- [x] `make teal` passes
- [x] `make test` passes
