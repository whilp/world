# build: add only= filter for all targets

Add unified `only=` parameter to filter all build targets (test, check, update, build, etc) by substring pattern.

- Makefile - add filter-only function and apply to all base aggregation variables

## Usage

```bash
make test only='skill'        # test files matching 'skill'
make check only='lib/skill'   # check files in lib/skill/
make update only='3p/rg'      # update only rg version
make test only='test_pr'      # test files with test_pr in name
```

## Implementation

The filter uses `findstring` for substring matching and is applied early during variable aggregation (`all_files`, `all_tests`, `all_versions`, `all_source_files`). All derived targets (`all_tested`, `all_astgreps`, etc) automatically respect the filter through the dependency chain.

Replaces the old `TEST` variable with a unified interface that works consistently across all targets.

## Validation

- [x] tests filtered correctly by pattern
- [x] check targets filtered correctly
- [x] update targets filtered correctly
- [x] derived variables auto-filter through dependencies
