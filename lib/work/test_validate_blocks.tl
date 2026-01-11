local lu = require("luaunit")

-- skip if posix not available (work module requires luaposix)
local has_posix = pcall(require, "posix")
if not has_posix then
  global function test_validate_blocks_skipped()
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
  validate_blocks: function(store: WorkStore, item_id: string, new_blocks: {string}): boolean, string
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

global TestValidateBlocks: {string:any} = {}

function TestValidateBlocks:setUp()
  store.reset(test_store)
end

function TestValidateBlocks:test_validate_blocks_with_nonexistent_id()
  add_work{
    id = "01TEST0000000000000000001",
    title = "existing item",
    created = "2025-12-01",
  }

  local item_id = "01TEST0000000000000000002"
  local nonexistent_id = "01TEST0000000000000000999"
  local blocks: {string} = { nonexistent_id }

  local ok, err = process.validate_blocks(test_store, item_id, blocks)

  lu.assertNil(ok)
  lu.assertNotNil(err)
  lu.assertStrContains(err, "block reference")
  lu.assertStrContains(err, nonexistent_id)
  lu.assertStrContains(err, "does not exist")
end

function TestValidateBlocks:test_validate_blocks_with_existing_id()
  add_work{
    id = "01TEST0000000000000000001",
    title = "item to block on",
    created = "2025-12-01",
  }

  local item_id = "01TEST0000000000000000002"
  local blocks: {string} = { "01TEST0000000000000000001" }

  local ok, err = process.validate_blocks(test_store, item_id, blocks)

  lu.assertTrue(ok)
  lu.assertNil(err)
end

function TestValidateBlocks:test_validate_blocks_with_multiple_nonexistent()
  add_work{
    id = "01TEST0000000000000000001",
    title = "existing item",
    created = "2025-12-01",
  }

  local item_id = "01TEST0000000000000000002"
  local blocks: {string} = { "01TEST0000000000000000001", "01TEST0000000000000000999" }

  local ok, err = process.validate_blocks(test_store, item_id, blocks)

  lu.assertNil(ok)
  lu.assertStrContains(err, "01TEST0000000000000000999")
  lu.assertStrContains(err, "does not exist")
end

function TestValidateBlocks:test_validate_blocks_empty_list()
  local item_id = "01TEST0000000000000000001"
  local blocks: {string} = {}

  local ok, err = process.validate_blocks(test_store, item_id, blocks)

  lu.assertTrue(ok)
  lu.assertNil(err)
end

function TestValidateBlocks:test_validate_blocks_self_reference()
  add_work{
    id = "01TEST0000000000000000001",
    title = "item",
    created = "2025-12-01",
  }

  local item_id = "01TEST0000000000000000001"
  local blocks: {string} = { item_id }

  local ok, err = process.validate_blocks(test_store, item_id, blocks)

  lu.assertNil(ok)
  lu.assertStrContains(err, "cannot block on itself")
end
