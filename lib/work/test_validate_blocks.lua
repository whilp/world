local lu = require("luaunit")

local data = require("work.data")
local process = require("work.process")
local store = require("work.store")
local Work = require("work.test_lib")
local test_store = Work.store

TestValidateBlocks = {}

function TestValidateBlocks:setUp()
  store.reset(test_store)
end

function TestValidateBlocks:test_validate_blocks_with_nonexistent_id()
  Work{
    id = "01TEST0000000000000000001",
    title = "existing item",
    created = "2025-12-01",
  }

  local item_id = "01TEST0000000000000000002"
  local nonexistent_id = "01TEST0000000000000000999"
  local blocks = { nonexistent_id }

  local ok, err = process.validate_blocks(test_store, item_id, blocks)

  lu.assertNil(ok)
  lu.assertNotNil(err)
  lu.assertStrContains(err, "block reference")
  lu.assertStrContains(err, nonexistent_id)
  lu.assertStrContains(err, "does not exist")
end

function TestValidateBlocks:test_validate_blocks_with_existing_id()
  Work{
    id = "01TEST0000000000000000001",
    title = "item to block on",
    created = "2025-12-01",
  }

  local item_id = "01TEST0000000000000000002"
  local blocks = { "01TEST0000000000000000001" }

  local ok, err = process.validate_blocks(test_store, item_id, blocks)

  lu.assertTrue(ok)
  lu.assertNil(err)
end

function TestValidateBlocks:test_validate_blocks_with_multiple_nonexistent()
  Work{
    id = "01TEST0000000000000000001",
    title = "existing item",
    created = "2025-12-01",
  }

  local item_id = "01TEST0000000000000000002"
  local blocks = { "01TEST0000000000000000001", "01TEST0000000000000000999" }

  local ok, err = process.validate_blocks(test_store, item_id, blocks)

  lu.assertNil(ok)
  lu.assertStrContains(err, "01TEST0000000000000000999")
  lu.assertStrContains(err, "does not exist")
end

function TestValidateBlocks:test_validate_blocks_empty_list()
  local item_id = "01TEST0000000000000000001"
  local blocks = {}

  local ok, err = process.validate_blocks(test_store, item_id, blocks)

  lu.assertTrue(ok)
  lu.assertNil(err)
end

function TestValidateBlocks:test_validate_blocks_self_reference()
  Work{
    id = "01TEST0000000000000000001",
    title = "item",
    created = "2025-12-01",
  }

  local item_id = "01TEST0000000000000000001"
  local blocks = { item_id }

  local ok, err = process.validate_blocks(test_store, item_id, blocks)

  lu.assertNil(ok)
  lu.assertStrContains(err, "cannot block on itself")
end

return TestValidateBlocks
