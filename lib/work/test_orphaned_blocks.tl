#!/usr/bin/env run-test.lua

-- skip if posix not available (work module requires luaposix)
local has_posix = pcall(require, "posix")
if not has_posix then
  error("SKIP requires luaposix")
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

local function test_find_items_blocking_on()
  store.reset(test_store)
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
  assert(#referencing == 2, "expected 2 referencing items, got " .. #referencing)

  local ids: {string} = {}
  for _, item in ipairs(referencing) do
    table.insert(ids, item.id)
  end
  table.sort(ids)

  assert(ids[1] == "01TEST0000000000000000002", "expected first id to match")
  assert(ids[2] == "01TEST0000000000000000003", "expected second id to match")
end
test_find_items_blocking_on()

local function test_find_items_blocking_on_no_references()
  store.reset(test_store)
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
  assert(#referencing == 0, "expected 0 referencing items, got " .. #referencing)
end
test_find_items_blocking_on_no_references()

local function test_find_items_blocking_on_nonexistent()
  store.reset(test_store)
  add_work{
    id = "01TEST0000000000000000001",
    title = "deploy to prod",
    created = "2025-12-01",
  }

  local referencing = process.find_items_blocking_on(test_store, "01TEST9999999999999999999")
  assert(#referencing == 0, "expected 0 referencing items, got " .. #referencing)
end
test_find_items_blocking_on_nonexistent()

local function test_find_items_blocking_on_multiple_blocks()
  store.reset(test_store)
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
  assert(#referencing_a == 1, "expected 1 referencing item for A, got " .. #referencing_a)
  assert(referencing_a[1].id == "01TEST0000000000000000003", "expected referencing item id to match")

  local referencing_b = process.find_items_blocking_on(test_store, "01TEST0000000000000000002")
  assert(#referencing_b == 1, "expected 1 referencing item for B, got " .. #referencing_b)
  assert(referencing_b[1].id == "01TEST0000000000000000003", "expected referencing item id to match")
end
test_find_items_blocking_on_multiple_blocks()
