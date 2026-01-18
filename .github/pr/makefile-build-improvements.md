# make: add build improvements for CI and maintainability

Enhances the Makefile and CI workflows with better performance and cleaner dependency handling.

- Makefile:7-9 - add --no-builtin-rules, --no-builtin-variables, --output-sync to MAKEFLAGS
- Makefile:142-143 - use .EXTRA_PREREQS for snapshot test dependencies
- .github/workflows/pr.yml:19-21 - add bash -x for command tracing
- .github/workflows/release.yml:32-38 - add bash -x for command tracing

## Changes

### --no-builtin-rules and --no-builtin-variables
Speeds up make startup and makes behavior more predictable by disabling implicit rules that the build system doesn't use.

### --output-sync
Prevents interleaved output in parallel builds, making logs readable when running with -j.

### bash -x in workflows
Use shell: bash -x {0} to trace all commands in CI logs for better debugging and reproducibility.

### .EXTRA_PREREQS for snapshot tests
Cleaner dependency declaration - $(build_snap) is now a prerequisite but excluded from $^, avoiding the need for $(word 2,$^) manipulation.

## Validation

- [x] make clean && make -j4 staged works
- [x] make -j4 files works
- [x] make -j4 test only=version passes
- [x] make -j4 test only=help passes (snapshot tests work)
- [x] make help works
