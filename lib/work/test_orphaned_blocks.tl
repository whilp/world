local lu = require("luaunit")

-- skip if posix not available (work module requires luaposix)
local has_posix = pcall(require, "posix")
if not has_posix then
  global function test_orphaned_blocks_skipped()
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
  validate: function(item: any): boolean, string
end

local record ProcessModule
  find_items_blocking_on: function(store: WorkStore, target_id: string): {WorkItem}
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

global TestOrphanedBlocks: {string:any} = {}

function TestOrphanedBlocks:setUp()
  store.reset(test_store)
end

function TestOrphanedBlocks:test_find_items_blocking_on()
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

  local referencing = process.find_items_blocking_on(test_store, "01TEST0000000000000000001")
  lu.assertEquals(#referencing, 2)

  local ids: {string} = {}
  for _, item in ipairs(referencing) do
    table.insert(ids, item.id)
  end
  table.sort(ids)

  lu.assertEquals(ids[1], "01TEST0000000000000000002")
  lu.assertEquals(ids[2], "01TEST0000000000000000003")
end

function TestOrphanedBlocks:test_find_items_blocking_on_no_references()
  add_work{
    id = "01TEST0000000000000000001",
    title = "deploy to prod",
    created = "2025-12-01",
  }

  add_work{
    id = "01TEST0000000000000000002",
    title = "implement auth",
    created = "2025-12-01",
  }

  local referencing = process.find_items_blocking_on(test_store, "01TEST0000000000000000001")
  lu.assertEquals(#referencing, 0)
end

function TestOrphanedBlocks:test_find_items_blocking_on_nonexistent()
  add_work{
    id = "01TEST0000000000000000001",
    title = "deploy to prod",
    created = "2025-12-01",
  }

  local referencing = process.find_items_blocking_on(test_store, "01TEST9999999999999999999")
  lu.assertEquals(#referencing, 0)
end

function TestOrphanedBlocks:test_find_items_blocking_on_multiple_blocks()
  add_work{
    id = "01TEST0000000000000000001",
    title = "feature A",
    created = "2025-12-01",
  }

  add_work{
    id = "01TEST0000000000000000002",
    title = "feature B",
    created = "2025-12-01",
  }

  add_work{
    id = "01TEST0000000000000000003",
    title = "deploy all features",
    created = "2025-12-01",
    blocks = { "01TEST0000000000000000001", "01TEST0000000000000000002" },
  }

  local referencing_a = process.find_items_blocking_on(test_store, "01TEST0000000000000000001")
  lu.assertEquals(#referencing_a, 1)
  lu.assertEquals(referencing_a[1].id, "01TEST0000000000000000003")

  local referencing_b = process.find_items_blocking_on(test_store, "01TEST0000000000000000002")
  lu.assertEquals(#referencing_b, 1)
  lu.assertEquals(referencing_b[1].id, "01TEST0000000000000000003")
end
