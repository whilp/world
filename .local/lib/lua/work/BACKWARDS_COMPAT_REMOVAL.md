# Removing backwards compatibility from work library

This document outlines the steps to remove backwards compatibility shims added during the refactor to introduce the unified API layer.

## Current state

The following modules have backwards-compatible function signatures:

- `data.lua` - Functions accept optional `store` parameter, default to `M.items`
- `process.lua` - Functions accept optional `store` parameter, default to `data.items`

These shims allow old code to continue working without changes.

## Affected code

### Test suite (~10 files in `.local/lib/lua/test/work/`)

All tests currently:
- Reset `data.items = {}` in `setUp()`
- Call library functions without store parameter
- Use `require("lib")` helper that writes to `data.items`

Example from `test/work/validate-blocks.lua`:
```lua
function TestValidateBlocks:setUp()
  data.items = {}  -- Direct module state access
end

-- Calls without store parameter
process.validate_blocks(item_id, blocks)
data.get("01TEST...")
```

### Neovim modules (may have direct library calls)

- `.config/nvim/lua/work/actions.lua` (12KB)
- `.config/nvim/lua/work/buffer.lua` (6.8KB)
- `.config/nvim/lua/work/picker.lua` (7.3KB)
- `.config/nvim/lua/work/complete.lua` (4.3KB)

Note: `init.lua` already uses the new API, but these modules might call library functions directly.

## Removal steps

### Phase 1: Update test suite

1. **Modify `test/work/lib.lua`** to use store instead of `data.items`:

```lua
local data = require("work.data")
local store = require("work.store")

-- Create a test store singleton
local _test_store = store.new()

local function Work(item)
  local ok, err = data.validate(item)
  if not ok then
    error("validation failed: " .. err)
  end

  if _test_store.items[item.id] then
    error("item id collision: " .. item.id)
  end

  store.add(_test_store, item)
end

-- Expose store for tests to reset
Work.store = _test_store

return Work
```

2. **Update each test file** to use store:

```lua
local Work = require("lib")
local test_store = Work.store

function TestValidateBlocks:setUp()
  store.reset(test_store)  -- Instead of data.items = {}
end

-- Update function calls to pass store
process.validate_blocks(test_store, item_id, blocks)
data.get(test_store, "01TEST...")
```

3. **Run test suite** to verify all tests pass:

```bash
cd .local/lib/lua/test/work
lua run.lua
```

### Phase 2: Audit nvim modules

1. **Search for direct library calls** in nvim modules:

```bash
cd .config/nvim/lua/work
rg 'require\("work\.(data|process)"\)' .
rg 'data\.(get|items|resolve_id|save)' .
rg 'process\.(enrich|get_ready|validate_blocks)' .
```

2. **For each direct call found:**
   - Option A: Replace with `api.*` call (preferred)
   - Option B: Update to pass store parameter

Example transformation:
```lua
-- Before (if found)
local data = require("work.data")
local item = data.get(id)

-- After (preferred)
local work = require("work")
local item, err = work.get(id)

-- Or (if api doesn't expose the function)
local api_internal = get_api()
local store_internal = api_internal._store
local item = data.get(store_internal, id)
```

3. **Test nvim integration** thoroughly:
   - `:Work list`
   - `:Work ready`
   - `:Work done <id>`
   - Test picker (`,wL`)
   - Test actions (`,wl`, `,wd`, etc.)

### Phase 3: Remove backwards compatibility shims

1. **Update `data.lua`:**

Remove `get_items()` helper:
```lua
-- DELETE THIS:
local function get_items(store_or_nil)
  if store_or_nil and type(store_or_nil) == "table" and store_or_nil.items then
    return store_or_nil.items
  end
  return M.items
end
```

Remove `M.items` or mark as truly deprecated:
```lua
-- DELETE OR CHANGE TO:
-- M.items = nil  -- REMOVED: use store parameter
```

Update all functions to require store parameter:
```lua
-- Before (backwards compatible)
M.get = function(store_or_id, maybe_id)
  local store, id
  if maybe_id then
    store = store_or_id
    id = maybe_id
  else
    store = nil
    id = store_or_id
  end
  local items = get_items(store)
  return items[id]
end

-- After (store required)
M.get = function(store, id)
  return store.items[id]
end
```

2. **Update `process.lua`:**

Remove `get_items()` helper (same as data.lua).

Update all functions to require store as first parameter:
```lua
-- Before
M.enrich = function(store_or_item, maybe_item)
  local store, item
  if maybe_item then
    store = store_or_item
    item = maybe_item
  else
    store = nil
    item = store_or_item
  end
  -- ...
end

-- After
M.enrich = function(store, item)
  -- ...
end
```

Update internal calls like `resolve_due_date_impl()` to always pass `items_table`.

3. **Update function signatures** throughout both files:

Functions to update in `data.lua`:
- `load_file(path, store, kinds)` - remove fallback logic
- `load_all(store, dir)` - make store required
- `get(store, id)` - make store required
- `get_all(store)` - make store required
- `get_by_file(store, source)` - make store required
- `resolve_id(store, short_id)` - make store required

Functions to update in `process.lua`:
- `get_transitive_dependencies(store, item_id)`
- `get_transitive_dependents(store, item_id)`
- `validate_blocks(store, item_id, new_blocks)`
- `resolve_due_date(store, item)`
- `get_unresolved_blocks(store, item)`
- `is_item_blocked(store, item)`
- `get_blocked_items(store)`
- `get_incomplete_items(store)`
- `get_ready_items(store)`
- `enrich(store, item)`
- `enrich_all(store, items)`
- `find_items_blocking_on(store, target_id)`

### Phase 4: Verification

1. **Run complete test suite:**

```bash
cd .local/lib/lua/test/work
lua run.lua
```

All tests must pass.

2. **Test CLI:**

```bash
work list
work ready --limit=5
work show <id>
work done <id>
work tree
```

3. **Test nvim integration:**

Open nvim and test all work commands and keybindings.

4. **Test with actual data:**

Use your real work items to ensure nothing breaks.

## Commit strategy

Make changes in separate commits for easier review/rollback:

1. Commit: "work: update test suite to use store parameter"
2. Commit: "work: audit and update nvim modules for store usage"
3. Commit: "work: remove backwards compatibility shims from data.lua and process.lua"

Each commit should be tested independently.

## Rollback plan

If issues are discovered after removal:

1. Revert the commits in reverse order
2. Investigate the specific issue
3. Fix the affected code
4. Retry the removal process

## Timeline estimate

- Phase 1 (tests): 1-2 hours
- Phase 2 (nvim audit): 1 hour
- Phase 3 (remove shims): 1 hour
- Phase 4 (verification): 30 minutes

Total: ~4 hours of focused work

## Benefits after removal

- Cleaner, more explicit API
- No ambiguous function signatures
- Better type clarity (store is always first parameter)
- Reduced code in data.lua and process.lua
- Forces all code to use the new architecture

## Notes

- The `api.lua` module already uses stores correctly and won't need changes
- The CLI and `nvim/init.lua` already use the new API exclusively
- Most of the work is mechanical updates to test files
