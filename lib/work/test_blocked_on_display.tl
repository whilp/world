local lu = require("luaunit")

-- skip if posix not available (work module requires luaposix)
local has_posix = pcall(require, "posix")
if not has_posix then
  global function test_blocked_on_display_skipped()
    lu.skip("requires luaposix")
  end
  return
end

local record WorkItemMeta
  source: string
  kind: string
end

local record WorkItem
  id: string
  title: string
  created: string
  blocks: {string}
  completed: string
  started: string
  description: string
  due: string
  priority: number
  log: {string:string}
  _meta: WorkItemMeta
end

local record WorkStore
  items: {string:WorkItem}
end

local record DataModule
  get: function(store: WorkStore, id: string): WorkItem
end

local record ProcessModule
  is_item_blocked: function(store: WorkStore, item: WorkItem): boolean
  get_unresolved_blocks: function(store: WorkStore, item: WorkItem): {string}
end

local record StoreModule
  reset: function(store: WorkStore): WorkStore
end

local record WorkModule
  store: WorkStore
end

local data = require("work.data") as DataModule
local process = require("work.process") as ProcessModule
local store = require("work.store") as StoreModule
local Work = require("work.test_lib") as WorkModule
local test_store = Work.store

-- Work callable helper
local work_call = (Work as {string:any}).__call or function(_: any, item: WorkItem)
  local ok, err = (require("work.data") as DataModule).validate(item as any)
  if not ok then
    error("validation failed: " .. (err as string))
  end
  if test_store.items[item.id] then
    error("item id collision: " .. item.id)
  end
  (require("work.store") as StoreModule).add(test_store, item)
end

local function add_work(item: WorkItem)
  local data_mod = require("work.data") as DataModule
  local store_mod = require("work.store") as StoreModule
  local ok, err = data_mod.validate(item as any)
  if not ok then
    error("validation failed: " .. (err as string))
  end
  if test_store.items[item.id] then
    error("item id collision: " .. item.id)
  end
  store_mod.add(test_store, item)
end

global TestBlockedOnDisplay: {string:any} = {}

function TestBlockedOnDisplay:setUp()
  store.reset(test_store)
end

function TestBlockedOnDisplay:test_unresolved_blocks_display()
  add_work{
    id = "01TEST0000000000000000001",
    title = "deploy to prod",
    created = "2025-12-01",
  }

  add_work{
    id = "01TEST0000000000000000002",
    title = "implement auth",
    created = "2025-12-01",
    blocks = { "01TEST0000000000000000001" },
  }

  add_work{
    id = "01TEST0000000000000000003",
    title = "write tests",
    created = "2025-12-01",
    blocks = { "01TEST0000000000000000001" },
  }

  local blocked_item = data.get(test_store, "01TEST0000000000000000001")

  lu.assertTrue(process.is_item_blocked(test_store, blocked_item))

  local unresolved = process.get_unresolved_blocks(test_store, blocked_item)

  lu.assertTrue(#unresolved > 0, "blocked item should show what items are blocking it, got: " .. #unresolved)
end
