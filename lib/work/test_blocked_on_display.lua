local lu = require("luaunit")

-- skip if posix not available (work module requires luaposix)
local has_posix = pcall(require, "posix")
if not has_posix then
  function test_blocked_on_display_skipped()
    lu.skip("requires luaposix")
  end
  return
end

local data = require("work.data")
local process = require("work.process")
local store = require("work.store")
local Work = require("work.test_lib")
local test_store = Work.store

TestBlockedOnDisplay = {}

function TestBlockedOnDisplay:setUp()
  store.reset(test_store)
end

function TestBlockedOnDisplay:test_unresolved_blocks_display()
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

  Work{
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

