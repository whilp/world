## Build workflow

```bash
make test              # run tests (incremental)
make clean test        # full rebuild and test
make test; make test   # verify incremental builds work (second should be instant)
make staged            # fetch and extract dependencies
make fetched           # fetch dependencies only
```

## Module patterns

Modules define in cook.mk:
- `module_version` - path to version.lua (3p modules)
- `module_files` - output files to build
- `module_tests` - test files
- `module_deps` - other modules this depends on

Auto-derived for modules with `_version`:
- `module_staged` - staged directory symlink (o/path/version.lua.staged)
- `module_dir` - same as `_staged`, use to reference files

```makefile
# 3p module cook.mk
modules += foo
foo_version := 3p/foo/version.lua
foo_tests := 3p/foo/test_foo.lua

# lib module cook.mk
modules += bar
bar_files := $(o)/bin/bar $(o)/lib/bar/init.lua
bar_tests := lib/bar/test_bar.lua
bar_deps := foo  # depends on foo_staged
```

Reference staged files using `_dir`:
```makefile
$(my_target): $(foo_staged)
	cp $(foo_dir)/some_file $@
```

## Test patterns

Tests have access to:
- `TEST_DIR` - module's staged directory (set by run-test.lua from TEST_DEPS)
- `TEST_BIN` - path to o/bin for binaries (cosmic, etc.)
- `TEST_TMPDIR` - temporary directory, cleaned after test

```lua
-- 3p module: find binary in staged dir
local bin = path.join(TEST_DIR, "sg")

-- lib module: find binary in TEST_BIN
local cosmic = path.join(os.getenv("TEST_BIN"), "cosmic")
```
