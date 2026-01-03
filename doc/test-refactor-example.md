# concrete refactoring example

## current state

### lib/build/cook.mk
```make
$(luatest_o)/lib/build/test_luafiles.lua.ok: $(manifest_git) $(manifest_luafiles) $(manifest_luatests)
$(luatest_o)/lib/build/test_luafiles.lua.ok: TEST_ARGS = $(manifest_git) $(manifest_luafiles) $(manifest_luatests)
```

### lib/build/test_luafiles.lua
```lua
local lu = require("luaunit")
local cosmo = require("cosmo")

local git_path = TEST_ARGS[1]
local luafiles_path = TEST_ARGS[2]
local luatests_path = TEST_ARGS[3]

-- what are these? have to look at cook.mk to understand
```

## improved version

### lib/build/cook.mk
```make
# name the inputs clearly
test_luafiles_inputs := $(manifest_git) $(manifest_luafiles) $(manifest_luatests)

$(luatest_o)/lib/build/test_luafiles.lua.ok: $(test_luafiles_inputs)
$(luatest_o)/lib/build/test_luafiles.lua.ok: TEST_ARGS = $(test_luafiles_inputs)
```

### lib/build/test_luafiles.lua
```lua
local lu = require("luaunit")
local cosmo = require("cosmo")
local test_args = require("test_args")

-- self-documenting: shows exactly what args this test expects
local args = test_args.parse({
  "manifest_git",
  "manifest_luafiles",
  "manifest_luatests",
})

-- named access is clearer than positional
local git_files = parse_null_separated(cosmo.Slurp(args.manifest_git))
local lua_files = read_lines(args.manifest_luafiles)
local lua_tests = read_lines(args.manifest_luatests)
```

## benefits

### in makefile
- **variable name documents intent**: `test_luafiles_inputs` tells you what these are
- **still explicit**: easy to grep and understand
- **minimal overhead**: just 1 extra line

### in test file
- **self-documenting**: function call shows what args are expected
- **validation**: errors if args are missing
- **named access**: `args.manifest_git` vs `TEST_ARGS[1]`
- **discoverable**: don't need to check cook.mk to understand test

### maintenance
- **easier to add args**: just add to list in both places
- **easier to reorder**: names don't depend on position
- **easier to debug**: clear error messages when args are wrong

## before/after comparison for all current TEST_ARGS usage

### lib/build/test_luacheck.lua

**current:**
```make
$(luatest_o)/lib/build/test_luacheck.lua.ok: TEST_ARGS = $(CURDIR)/$(luacheck_config)
```
```lua
-- test has to know this is a config path
local config = TEST_ARGS[1]
```

**improved:**
```make
test_luacheck_config := $(CURDIR)/$(luacheck_config)
$(luatest_o)/lib/build/test_luacheck.lua.ok: TEST_ARGS = $(test_luacheck_config)
```
```lua
local args = test_args.parse({"luacheck_config"})
local config = args.luacheck_config
```

### lib/build/test_ast_grep.lua

**current:**
```make
$(luatest_o)/lib/build/test_ast_grep.lua.ok: TEST_ARGS = $(CURDIR)/$(astgrep_config) $(CURDIR)/.ast-grep
```
```lua
local config = TEST_ARGS[1]
local rules_dir = TEST_ARGS[2]  -- or is it a file? unclear
```

**improved:**
```make
test_ast_grep_inputs := $(CURDIR)/$(astgrep_config) $(CURDIR)/.ast-grep
$(luatest_o)/lib/build/test_ast_grep.lua.ok: TEST_ARGS = $(test_ast_grep_inputs)
```
```lua
local args = test_args.parse({"astgrep_config", "astgrep_rules_dir"})
local config = args.astgrep_config
local rules_dir = args.astgrep_rules_dir  -- clearly a directory
```

## even simpler alternative: just improve test files

if makefile duplication is acceptable, we could just improve discoverability in tests:

```lua
-- current
local git_path = TEST_ARGS[1]
local luafiles_path = TEST_ARGS[2]
local luatests_path = TEST_ARGS[3]

-- improved with just better naming and comments
-- test arguments (see lib/build/cook.mk for values):
--   [1] manifest_git     - null-separated git ls-files output
--   [2] manifest_luafiles - newline-separated lua file list
--   [3] manifest_luatests - newline-separated lua test list
local manifest_git = TEST_ARGS[1]
local manifest_luafiles = TEST_ARGS[2]
local manifest_luatests = TEST_ARGS[3]
```

this gives you discoverability without any new infrastructure

## recommendation

1. **immediate**: add descriptive comments in test files documenting TEST_ARGS
2. **next**: create named variables in cook.mk files for clarity
3. **optional**: add test_args helper module for validation and named access

prioritize discoverability - future maintainers (including yourself) will thank you
