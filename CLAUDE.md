## Build workflow

```bash
make test              # run tests (incremental)
make clean test        # full rebuild and test
make test; make test   # verify incremental builds work (second should be instant)
make staged            # fetch and extract dependencies
make fetched           # fetch dependencies only
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
