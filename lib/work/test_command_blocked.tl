local lu = require("luaunit")

-- skip if posix not available (work module requires luaposix)
local has_posix = pcall(require, "posix")
if not has_posix then
  global function test_command_blocked_skipped()
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
  validate: function(item: any): boolean, string
end

local record ProcessModule
  get_blocked_items: function(store: WorkStore): {WorkItem}
end

local record StoreModule
  reset: function(store: WorkStore): WorkStore
  add: function(store: WorkStore, item: WorkItem): WorkStore
end

local record WorkModule
  store: WorkStore
end

local data = require("work.data") as DataModule
local process = require("work.process") as ProcessModule
local store = require("work.store") as StoreModule
local Work = require("work.test_lib") as WorkModule
local test_store = Work.store

local function add_work(item: WorkItem)
  local ok, err = data.validate(item as any)
  if not ok then
    error("validation failed: " .. (err as string))
  end
  if test_store.items[item.id] then
    error("item id collision: " .. item.id)
  end
  store.add(test_store, item)
end

global TestCommandBlocked: {string:any} = {}

function TestCommandBlocked:setUp()
  store.reset(test_store)
end

function TestCommandBlocked:test_blocked_items_list()
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

  local item1 = data.get(test_store, "01TEST0000000000000000001")

  local blocked_items = process.get_blocked_items(test_store)
  lu.assertEquals(#blocked_items, 1)
  lu.assertEquals(blocked_items[1].id, item1.id)
end
