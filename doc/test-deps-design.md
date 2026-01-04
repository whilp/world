# test dependency tracking design

## problem

tests depend on files but those dependencies aren't tracked by make, leading to:
- manual .ok target definitions in cook.mk files
- dependencies getting out of sync with actual test requirements
- no automatic rebuild when dependencies change

## approach 1: auto-generated .deps files (like gcc -MD)

### concept

tests declare dependencies via a function:
```lua
local function dependencies()
  return {
    "lib/build/review.lua",
    "$(o_any)/build/lib/build/review.lua",
  }
end

-- export for test runner
local cosmo = require("cosmo")
if cosmo.is_main() then
  -- normal test execution
else
  return {dependencies = dependencies}
end
```

test runner generates:
```make
# o/luatest/lib/build/test_review.lua.deps
o/luatest/lib/build/test_review.lua.ok: lib/build/review.lua
o/luatest/lib/build/test_review.lua.ok: $(o_any)/build/lib/build/review.lua
```

Makefile includes them:
```make
-include $(patsubst %.ok,%.deps,$(luatest_files))
```

### pros
- dependencies live with test code
- no manual cook.mk maintenance
- standard make idiom (like .d files for C)

### cons
- bootstrap problem: deps file doesn't exist on first run
- needs runner modification
- dependencies evaluated twice (once for .deps, once for test)
- make variables in strings are awkward

## approach 2: convention-based dependencies

### concept

automatic rules based on file location:
```make
# tests in lib/foo/ automatically depend on lib/foo/*.lua (excluding tests)
$(luatest_o)/lib/%/test*.lua.ok: lib/%/*.lua
```

### pros
- zero configuration for common case
- simple, predictable behavior

### cons
- over-approximation (depends on files it might not use)
- doesn't handle cross-module dependencies
- still need explicit rules for special cases

## approach 3: runtime discovery

### concept

test runner uses debug hooks to track which files get loaded, writes them to .deps:
```lua
local loaded = {}
debug.sethook(function()
  -- track require() calls
end, "c")
```

### pros
- completely automatic
- accurate - only actual dependencies

### cons
- complex implementation
- performance overhead
- may miss conditional requires
- doesn't help with non-lua dependencies (binaries, data files)

## approach 4: annotation-based

### concept

tests declare deps in comments:
```lua
--depends: lib/build/review.lua
--depends: $(o_any)/build/lib/build/review.lua
```

build script scans for these and generates .deps files

### pros
- simple to implement
- explicit and clear
- no need to execute test code

### cons
- non-standard syntax
- easy to forget to update
- string-based make variable references

## recommendation

start with **approach 2 (convention-based)** for 80% case, then add **approach 1 (function-based)** for special cases:

```lua
-- most tests: convention handles it automatically
-- special cases: opt-in function
function test_dependencies()
  return {
    manifest_git,     -- reference make variable by name
    manifest_luafiles,
    manifest_luatests,
  }
end
```

benefits:
- zero config for common case
- escape hatch for complex cases
- make variable names not strings
- simpler than full .deps generation

implementation:
1. add convention rule to Makefile
2. add optional deps function support to test runner
3. generate .deps only for tests with the function
