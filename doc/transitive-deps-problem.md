# transitive dependency problem

## the problem

### current state
```make
# we specify direct dependency
$(luatest_o)/lib/build/test_review.lua.ok: lib/build/review.lua
```

### what's missing
```lua
-- lib/build/review.lua
local cosmo = require("cosmo")
local spawn = require("cosmic.spawn")
local unix = require("cosmo.unix")
```

if `cosmic.spawn` changes, `test_review.lua.ok` should rebuild, but it won't because we don't track the transitive dependencies.

### dependency graph
```
test_review.lua
  └─ build.review (lib/build/review.lua)
       ├─ cosmo (builtin)
       ├─ cosmic.spawn (lib/cosmic/spawn.lua)
       └─ cosmo.unix (builtin)
```

we only track the first level, missing `lib/cosmic/spawn.lua`.

## real example: lib/build/test_luafiles.lua

```lua
-- lib/build/test_luafiles.lua
local lu = require("luaunit")           -- o/any/luaunit/lib/luaunit.lua
local cosmo = require("cosmo")          -- builtin (APE)

-- test uses lib/build/manifest.lua via command args
-- manifest.lua requires:
local spawn = require("cosmic.spawn")   -- lib/cosmic/spawn.lua
local unix = require("cosmo.unix")      -- builtin

-- so the REAL dependencies are:
-- lib/build/test_luafiles.lua.ok:
--   - lib/build/manifest.lua (direct)
--   - lib/cosmic/spawn.lua (transitive via manifest)
--   - o/any/luaunit/lib/luaunit.lua (direct via require)
--   - manifest files (data dependencies)
```

### current makefile
```make
$(luatest_o)/lib/build/test_luafiles.lua.ok: $(manifest_git) $(manifest_luafiles) $(manifest_luatests)
$(luatest_o)/lib/build/test_luafiles.lua.ok: TEST_ARGS = $(manifest_git) $(manifest_luafiles) $(manifest_luatests)
```

**missing:**
- `lib/build/manifest.lua` (specified via TEST_ARGS but not as dependency!)
- `lib/cosmic/spawn.lua` (transitive)
- `o/any/luaunit/lib/luaunit.lua` (transitive)

## how bad is this?

check if we're missing direct deps:
```bash
# find what a test requires
$ grep "^local .* = require" lib/build/test_luafiles.lua
local lu = require("luaunit")
local cosmo = require("cosmo")

# check if manifest.lua is a dependency
$ rg "test_luafiles.*manifest.lua" lib/build/cook.mk
# NOT FOUND - we're missing direct dependency on manifest.lua!
```

## approaches to solve this

### 1. static dependency scanning (like C header scanning)

scan lua files for `require()` calls and generate .deps files:

```bash
# extract_lua_deps.lua
$ lua extract_lua_deps.lua lib/build/test_luafiles.lua
lib/build/test_luafiles.lua.ok: o/any/luaunit/lib/luaunit.lua
lib/build/test_luafiles.lua.ok: lib/cosmo/init.lua
```

**pros:**
- automatic
- complete (finds all requires)
- standard make pattern

**cons:**
- complex: need to resolve require paths to files
- doesn't handle dynamic requires: `require(variable)`
- bootstrap problem

### 2. runtime dependency tracking

use debug hooks to track what files get loaded:

```lua
-- in test runner
local loaded_files = {}
debug.sethook(function()
  local info = debug.getinfo(2, "S")
  if info.source:match("^@") then
    loaded_files[info.source:sub(2)] = true
  end
end, "c")

-- run test
require(test_module)

-- write .deps file
write_deps(loaded_files)
```

**pros:**
- accurate - only actual dependencies
- handles dynamic requires
- no parsing needed

**cons:**
- requires instrumented test run
- performance overhead
- dependencies only discovered after first run

### 3. convention-based wildcards

depend on entire module directories:

```make
# test depends on all lua files in the module
$(luatest_o)/lib/build/%.ok: lib/build/*.lua

# test depends on all lib files
$(luatest_o)/lib/build/%.ok: lib/**/*.lua
```

**pros:**
- simple
- no tracking infrastructure
- over-approximates (safe)

**cons:**
- rebuilds more than necessary
- doesn't work for cross-module deps

### 4. manual specification with includes

```make
# lib/build/test_luafiles.deps (checked in or generated)
lib/build/test_luafiles.lua.ok: lib/build/manifest.lua
lib/build/test_luafiles.lua.ok: lib/cosmic/spawn.lua
lib/build/test_luafiles.lua.ok: o/any/luaunit/lib/luaunit.lua

# in Makefile
-include lib/build/*.deps
```

**pros:**
- explicit
- versionable
- works with manual maintenance

**cons:**
- manual
- easy to forget

### 5. module-level dependencies

instead of file-level, track at module level:

```make
# lib/build/cook.mk declares what it needs
build_module_deps := cosmic

# automatically includes deps
$(luatest_o)/lib/build/%.ok: $(foreach dep,$(build_module_deps),lib/$(dep)/*.lua)
```

**pros:**
- coarse-grained is easier
- module boundaries are natural
- less maintenance

**cons:**
- over-approximates within modules
- still need to declare cross-module deps

## hybrid recommendation

### level 1: fix missing direct dependencies

```make
# lib/build/cook.mk
test_luafiles_inputs := $(manifest_git) $(manifest_luafiles) $(manifest_luatests)

# BEFORE:
$(luatest_o)/lib/build/test_luafiles.lua.ok: $(test_luafiles_inputs)

# AFTER: add direct file dependencies
$(luatest_o)/lib/build/test_luafiles.lua.ok: lib/build/test_luafiles.lua $(manifest_script) $(test_luafiles_inputs)
```

this is already happening via the base rule! but cook.mk overrides can add more.

### level 2: add module-level dependencies

```make
# lib/build/cook.mk declares module dependencies
build_test_deps := $(cosmic_lib)/cosmic/spawn.lua

# tests in lib/build/ depend on these
$(luatest_o)/lib/build/%.ok: $(build_test_deps)
```

### level 3: use convention for in-module deps

```make
# all tests in lib/build/ depend on all lib/build/*.lua
$(luatest_o)/lib/build/test_%.ok: lib/build/*.lua
```

### level 4 (optional): runtime tracking for verification

run tests with instrumentation, generate .deps, compare with manual:

```bash
$ make test TRACK_DEPS=1
# generates .deps files from runtime

$ diff lib/build/test_luafiles.lua.deps lib/build/test_luafiles.lua.deps.tracked
# shows if we're missing any
```

## practical first steps

1. **audit current cook.mk files** - find missing direct deps
2. **add convention rules** - depend on same-module files
3. **document module deps** - add comments listing cross-module deps
4. **consider build tool** - simple require() scanner

## checking current state

```bash
# find tests that import from other modules
$ rg 'require\("(?!luaunit)(\w+)\.' lib/*/test*.lua

# for each, check if cook.mk specifies the dependency
$ rg 'test_foo.*cosmic' lib/build/cook.mk
```

let me check this now...
