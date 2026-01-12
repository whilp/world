# build: add makefile validation test

Tests the actual repo Makefile using GNU make's built-in validation features.

## Changes

- `lib/build/test_makefile.tl` - tests real repo makefiles at runtime

## Approach

Tests run make commands against the actual repo Makefile:
- `make -n <target>` - dry run validates syntax and prerequisites
- `make -p -n -q` - database dump validates variables and rules are defined

## Checks tested

| Test | Validates |
|------|-----------|
| `test_dry_run_succeeds` | `make -n files` passes |
| `test_help_dry_run` | `make -n help` passes |
| `test_test_dry_run` | `make -n test` passes |
| `test_check_dry_run` | `make -n check` passes |
| `test_print_database` | `make -p` outputs expected variables |
| `test_all_major_targets` | All major targets can be dry-run |

## Validation

- [x] `make o/lib/build/test_makefile.tl.test.ok` passes
