local lu = require("luaunit")

-- skip if posix not available (work module requires luaposix)
local has_posix = pcall(require, "posix")
if not has_posix then
  function test_command_blocked_skipped()
    lu.skip("requires luaposix")
  end
  return
end

local data = require("work.data")
local process = require("work.process")
local store = require("work.store")
local Work = require("work.test_lib")
local test_store = Work.store

TestCommandBlocked = {}

function TestCommandBlocked:setUp()
  store.reset(test_store)
end

function TestCommandBlocked:test_blocked_items_list()
  Work{
    id = "01TEST0000000000000000001",
    title = "deploy to prod",
    created = "2025-12-01",
  }

  Work{
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

