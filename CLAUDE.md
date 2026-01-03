## Build workflow

```bash
make test              # run tests (incremental)
make clean test        # full rebuild and test
make test; make test   # verify incremental builds work (second should be instant)
make staged            # fetch and extract dependencies
make fetched           # fetch dependencies only
```

## Output structure

Each module gets a directory under `o/`:

```
o/<module>/
  .versioned  -> 3p/<module>/version.lua
  .fetched    -> ../fetched/<module>/<ver>-<sha>/
  .staged     -> ../staged/<module>/<ver>-<sha>/
  .built/     (lib modules only, for build staging)
```

Actual files live in:
- `o/fetched/<module>/<ver>-<sha>/` - downloaded archives
- `o/staged/<module>/<ver>-<sha>/` - extracted files

## Module patterns

Modules define in cook.mk:
- `module_version` - path to version.lua (3p modules)
- `module_files` - output files to build
- `module_tests` - test files
- `module_deps` - other modules this depends on

Auto-derived for modules with `_version`:
- `module_staged` - symlink `o/<module>/.staged`
- `module_dir` - same as `_staged`, resolves to extracted files

```makefile
# 3p module cook.mk
modules += foo
foo_version := 3p/foo/version.lua
foo_srcs := foo.lua         # optional: document staged files
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
	cp $(foo_dir)/*.lua $@
```

## version.lua options

```lua
return {
  version = "1.0.0",
  format = "tar.gz",           -- or "zip", "binary"
  strip_components = 1,        -- strip N levels from archive
  strip_prefix = "src",        -- optional: flatten subdirectory
  url = "https://example.com/{version}.tar.gz",
  platforms = {
    ["*"] = { sha = "..." },   -- or per-platform
    ["linux-x86_64"] = { sha = "..." },
  },
}
```

## Test patterns

Tests have access to:
- `TEST_DIR` - module's staged directory (set by Makefile)
- `TEST_BIN` - path to o/bin for binaries (cosmic, etc.)
- `TEST_TMPDIR` - temporary directory, cleaned after test

```lua
-- 3p module: find binary in staged dir
local bin = path.join(TEST_DIR, "sg")

-- lib module: find binary in TEST_BIN
local cosmic = path.join(os.getenv("TEST_BIN"), "cosmic")
```
