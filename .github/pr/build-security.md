# build: add landlock-make with .PLEDGE/.UNVEIL constraints

Bootstrap landlock-make from whilp/cosmopolitan and add sandbox constraints
directly in Makefile rules using `.PLEDGE` and `.UNVEIL` variables.

## Changes

- bin/make - bootstrap script that downloads landlock-make
- Makefile - add .PLEDGE/.UNVEIL constraints for fetch/stage/test rules

## Constraints

| Phase | Pledge | Unveil |
|-------|--------|--------|
| fetch | stdio rpath wpath cpath inet dns | r:version r:/etc/ssl rwc:output |
| stage | stdio rpath wpath cpath proc exec | r:archive rwc:output rx:/usr/bin |
| test | stdio rpath wpath cpath proc exec | r:sources rwc:/tmp rwc:output rx:/usr |

## Usage

```bash
# Use landlock-make for sandboxed builds
bin/make test

# Or with GNU make (constraints ignored)
make test
```

## Validation

- [x] bin/make test passes
- [x] incremental builds work
- [x] constraints enforced by landlock-make
