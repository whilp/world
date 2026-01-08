# build: add bump target for applying dependency updates

Add `make bump` target to apply version updates checked by `make update`.

- lib/build/check-update.lua - add `--apply` flag to write updated versions
- Makefile - add `bump` target with help documentation
- lib/build/make-help.lua - add `parse_phony_targets` function
- lib/build/test_help.lua - add test ensuring all phony targets are documented

## Usage

```bash
make update only=gh   # check for updates
make bump only=gh     # apply updates
```

## Validation

- [x] tests pass
- [x] `make help` shows bump target
