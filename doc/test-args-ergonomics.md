# making test dependencies and args more ergonomic

## current pain points

### duplication
```make
$(luatest_o)/lib/build/test_luafiles.lua.ok: $(manifest_git) $(manifest_luafiles) $(manifest_luatests)
$(luatest_o)/lib/build/test_luafiles.lua.ok: TEST_ARGS = $(manifest_git) $(manifest_luafiles) $(manifest_luatests)
```
we list the same files twice: once for dependency, once to pass to test

### discoverability in test code
```lua
local git_path = TEST_ARGS[1]      -- what is this?
local luafiles_path = TEST_ARGS[2]  -- what is this?
local luatests_path = TEST_ARGS[3]  -- what is this?
```
hard to know what args a test expects without looking at cook.mk

## approach 1: use make automatic variables

### idea: derive TEST_ARGS from dependencies

```make
# define which deps should be passed as args
$(luatest_o)/lib/build/test_luafiles.lua.ok: PASS_DEPS = yes
$(luatest_o)/lib/build/test_luafiles.lua.ok: $(manifest_git) $(manifest_luafiles) $(manifest_luatests)

# in base rule, automatically set TEST_ARGS if PASS_DEPS is set
$(luatest_o)/%.ok: % $(luatest_script) $(luaunit) $(script_deps)
	$(TEST_ENV) $(luatest_runner) $< $@ $(if $(PASS_DEPS),$(filter-out $< $(luatest_script) $(luaunit) $(script_deps),$^),$(TEST_ARGS))
```

**pros**: no duplication
**cons**: complex filter expression, less explicit

## approach 2: define macro for deps+args

```make
# macro that sets both dependency and args
define test_with_files
$(luatest_o)/$(1).ok: $(2)
$(luatest_o)/$(1).ok: TEST_ARGS = $(2)
endef

# use it
$(eval $(call test_with_files,lib/build/test_luafiles.lua,$(manifest_git) $(manifest_luafiles) $(manifest_luatests)))
```

**pros**: single line, DRY
**cons**: less readable, harder to grep

## approach 3: variable naming convention

```make
# define args as variable
test_luafiles_args := $(manifest_git) $(manifest_luafiles) $(manifest_luatests)

# use variable for both
$(luatest_o)/lib/build/test_luafiles.lua.ok: $(test_luafiles_args)
$(luatest_o)/lib/build/test_luafiles.lua.ok: TEST_ARGS = $(test_luafiles_args)
```

**pros**: explicit, gives name to the set of args
**cons**: still two lines, extra variable

## approach 4: document args in test file

### current
```lua
local git_path = TEST_ARGS[1]
local luafiles_path = TEST_ARGS[2]
local luatests_path = TEST_ARGS[3]
```

### improved: helper function with documentation

```lua
-- parse_test_args: extract test arguments with names
-- args expected: manifest_git manifest_luafiles manifest_luatests
local function parse_test_args(names)
  local result = {}
  for i, name in ipairs(names) do
    result[name] = TEST_ARGS[i]
  end
  return result
end

local args = parse_test_args({"manifest_git", "manifest_luafiles", "manifest_luatests"})
local git_path = args.manifest_git
local luafiles_path = args.manifest_luafiles
```

**pros**: self-documenting, named access
**cons**: more verbose, slight overhead

### even better: validation helper

```lua
local test_args = require("test_args")  -- hypothetical helper module

-- validates count and provides named access
local args = test_args.parse({
  "manifest_git",
  "manifest_luafiles",
  "manifest_luatests",
})

-- usage
local git_path = args.manifest_git
```

## approach 5: use make variables in test via env

```make
$(luatest_o)/lib/build/test_luafiles.lua.ok: $(manifest_git) $(manifest_luafiles) $(manifest_luatests)
$(luatest_o)/lib/build/test_luafiles.lua.ok: TEST_ENV = \
  MANIFEST_GIT=$(manifest_git) \
  MANIFEST_LUAFILES=$(manifest_luafiles) \
  MANIFEST_LUATESTS=$(manifest_luatests)
```

```lua
-- in test
local git_path = os.getenv("MANIFEST_GIT")
local luafiles_path = os.getenv("MANIFEST_LUAFILES")
local luatests_path = os.getenv("MANIFEST_LUATESTS")
```

**pros**: named variables, no positional args
**cons**: still duplication in makefile, env vars are global

## approach 6: automatic dependency-to-args for specific patterns

```make
# pattern: manifest files are both deps and args
manifest_deps := $(manifest_git) $(manifest_luafiles) $(manifest_luatests)

# dependency
$(luatest_o)/lib/build/test_luafiles.lua.ok: $(manifest_deps)

# base rule automatically passes any variables ending in _deps
$(luatest_o)/%.ok: % $(luatest_script) $(luaunit) $(script_deps)
	$(TEST_ENV) $(luatest_runner) $< $@ $(TEST_ARGS) $($(*)_deps)
```

**pros**: convention-based, less duplication
**cons**: magic behavior, harder to understand

## recommendation: combination approach

### for makefile (approach 3 + better naming)

```make
# name the dependency set clearly
test_luafiles_files := $(manifest_git) $(manifest_luafiles) $(manifest_luatests)

$(luatest_o)/lib/build/test_luafiles.lua.ok: $(test_luafiles_files)
$(luatest_o)/lib/build/test_luafiles.lua.ok: TEST_ARGS = $(test_luafiles_files)
```

benefits:
- explicit and greppable
- self-documenting variable name
- only 3 lines instead of 2, but much clearer

### for test files (approach 4 - helper function)

create `lib/test_args.lua`:
```lua
-- helper to parse and validate test arguments
local M = {}

function M.parse(spec)
  local args = {}
  for i, name in ipairs(spec) do
    local value = TEST_ARGS[i]
    if not value then
      error(string.format("missing test argument %d: %s", i, name))
    end
    args[name] = value
  end
  return args
end

return M
```

use in tests:
```lua
local test_args = require("test_args")

local args = test_args.parse({
  "manifest_git",
  "manifest_luafiles",
  "manifest_luatests",
})

-- clear named access
local git_files = read_git_files(args.manifest_git)
local lua_files = read_lua_files(args.manifest_luafiles)
```

benefits:
- self-documenting: function call shows what args are expected
- validates arg count
- named access instead of positional
- easy to add defaults or type checking later

## comparison with current state

### current (4 locations with duplication)
```
lib/build/cook.mk
├─ test_luacheck: $(luacheck_config)
├─ test_ast_grep: $(astgrep_config) $(astgrep_rules)
└─ test_luafiles: $(manifest_git) $(manifest_luafiles) $(manifest_luatests)

3p/ast-grep/cook.mk
└─ test_rules: $(astgrep_config) $(astgrep_rules)
```

### with improvements (named vars + helper module)
```make
# lib/build/cook.mk
test_luafiles_inputs := $(manifest_git) $(manifest_luafiles) $(manifest_luatests)
$(luatest_o)/lib/build/test_luafiles.lua.ok: $(test_luafiles_inputs)
$(luatest_o)/lib/build/test_luafiles.lua.ok: TEST_ARGS = $(test_luafiles_inputs)
```

```lua
-- lib/build/test_luafiles.lua
local test_args = require("test_args")
local args = test_args.parse({"manifest_git", "manifest_luafiles", "manifest_luatests"})
```

result:
- makefile: slightly more verbose but much clearer
- test: self-documenting what args it expects
- both: easy to find and update together
