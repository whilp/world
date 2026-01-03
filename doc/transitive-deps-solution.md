# solving transitive dependencies: practical approach

## findings from analysis

ran `doc/analyze-test-deps.lua` and found:

### direct dependencies work
tests in `lib/build/` require modules from `lib/build/` - these work via the base rule:
```make
$(luatest_o)/%.ok: % $(luatest_script) $(luaunit) $(script_deps)
```

the `%` matches `lib/build/test_review.lua`, so the test depends on itself (direct file dep).

### missing: same-module file dependencies

when `lib/build/test_review.lua` changes, test reruns ✓
when `lib/build/review.lua` changes, test should rerun but doesn't ✗

```lua
-- lib/build/test_review.lua
local review = require("build.review")  -- lib/build/review.lua
```

we need: `test_review.lua.ok: lib/build/review.lua`

### missing: cross-module dependencies

many lib/build tests use cosmic.spawn:
```lua
-- lib/build/test_luacheck.lua
local spawn = require("cosmic.spawn")  -- lib/cosmic/spawn.lua
```

when `lib/cosmic/spawn.lua` changes, these tests should rerun but don't.

## practical solution: three levels

### level 1: same-module convention (automatic, safe)

add to Makefile:
```make
# convention: tests in lib/foo/ depend on all lib/foo/*.lua
# (excluding test files since they depend on themselves via base rule)
$(luatest_o)/lib/%/test_%.lua.ok: lib/%/*.lua
```

**effect:**
- `lib/build/test_review.lua.ok` now depends on all `lib/build/*.lua`
- automatic, no maintenance
- slight over-approximation (rebuilds if any file changes)

**verification:**
```bash
$ make -n o/luatest/lib/build/test_review.lua.ok | grep -o 'lib/build/[^ ]*\.lua'
# should show lib/build/*.lua in prerequisites
```

### level 2: cross-module dependencies (explicit)

add to Makefile:
```make
# lib/build tests use cosmic modules
cosmic_files := $(wildcard lib/cosmic/*.lua)
$(luatest_o)/lib/build/test_%.lua.ok: $(cosmic_files)
```

**effect:**
- all lib/build tests now depend on lib/cosmic changes
- explicit but manageable
- also over-approximates (not all tests use cosmic)

**alternative:** be more specific per cook.mk
```make
# in lib/build/cook.mk
# tests that use cosmic.spawn
cosmic_spawn_tests := test_luacheck.lua test_ast_grep.lua
$(addprefix $(luatest_o)/lib/build/,$(cosmic_spawn_tests:.lua=.lua.ok)): $(cosmic_lib)/cosmic/spawn.lua
```

### level 3: transitive via runtime tracking (optional, for verification)

create `lib/build/track-deps.lua`:
```lua
-- track what files get loaded during test run
local loaded = {}

local old_loadfile = loadfile
function loadfile(path)
  if path then
    loaded[path] = true
  end
  return old_loadfile(path)
end

-- hook package.searchers to track requires
-- ... (similar tracking)

-- at end, write .deps.actual file
at_exit(function()
  local f = io.open(output_file, "w")
  for file in pairs(loaded) do
    f:write(file .. "\n")
  end
  f:close()
end)
```

**use:**
```bash
$ TRACK_DEPS=1 make test
# generates .deps.actual files
$ diff lib/build/test_review.lua.deps lib/build/test_review.lua.deps.actual
# shows if manual .deps is missing anything
```

## recommended immediate action

### patch 1: add same-module convention

```make
# in Makefile, after the base luatest rule

# convention: tests depend on all lua files in same module directory
# this handles intra-module dependencies automatically
define lib_test_deps
$(luatest_o)/lib/$(1)/test_%.lua.ok: lib/$(1)/*.lua
endef

# apply to all lib modules (extract from directory listing)
lib_modules := $(patsubst lib/%/,%,$(sort $(dir $(wildcard lib/*/))))
$(foreach mod,$(lib_modules),$(eval $(call lib_test_deps,$(mod))))
```

### patch 2: add cross-module deps for cosmic

```make
# lib/build and other modules use lib/cosmic
cosmic_files := $(wildcard lib/cosmic/*.lua)
$(luatest_o)/lib/build/test_%.lua.ok: $(cosmic_files)
$(luatest_o)/lib/work/test_%.lua.ok: $(cosmic_files)
```

## verification

after applying patches:
```bash
# check that dependencies are registered
$ make -p | grep 'lib/build/test_review.lua.ok:'

# should show:
# - lib/build/test_review.lua (base rule)
# - lib/build/*.lua (same-module convention)
# - lib/cosmic/*.lua (cross-module explicit)
```

## comparison with other approaches

### C/C++ with gcc -MD
- compiler tracks headers at compile time
- generates .d files: `foo.o: foo.c foo.h bar.h`
- perfect accuracy, automatic
- **our equivalent:** runtime tracking (level 3)

### Python with import hooks
- can track imports at runtime
- tools like `pydeps` do static analysis
- **our equivalent:** static require() scanning or runtime tracking

### Node.js/bundlers
- static analysis of `require()`/`import`
- dependency graphs built into tooling
- **our equivalent:** static analysis tool

### Make conventions
- pattern rules + wildcards
- conservative (over-approximate)
- simple, predictable
- **our equivalent:** same-module convention (level 1)

## trade-offs

| approach | accuracy | complexity | maintenance |
|----------|----------|------------|-------------|
| same-module wildcard | medium | low | zero |
| explicit cross-module | high | medium | manual |
| runtime tracking | perfect | high | automated |
| static analysis | high | medium | automated |

**recommendation:** start with same-module (low cost, good enough), add explicit cross-module as needed

## potential issues

### pattern rule priority
multiple pattern rules might conflict:
```make
# which rule matches lib/build/test_review.lua.ok?
$(luatest_o)/%.ok: %                           # base rule
$(luatest_o)/lib/%/test_%.lua.ok: lib/%/*.lua  # convention rule
```

**answer:** both match, make uses both prerequisites (union)

### performance
depending on `lib/cosmic/*.lua` means all tests rebuild when any cosmic file changes.

**mitigation:** only add cross-module deps where actually needed

### wildcard expansion timing
```make
cosmic_files := $(wildcard lib/cosmic/*.lua)  # evaluated at makefile parse time
```

if you add a new cosmic file, make won't see it until re-run.

**mitigation:** acceptable - adding new files is rare, re-running make is normal

## next steps

1. **implement level 1** (same-module convention) - 5 lines in Makefile
2. **identify cross-module deps** - grep for requires across module boundaries
3. **add level 2** (explicit cross-module) - a few lines per dependency
4. **(optional) implement level 3** - for validation/CI
