local lu = require("luaunit")

local data = require("work.data")
local process = require("work.process")
local Work = require("lib")

TestBlockers = {}

function TestBlockers:setUp()
  data.items = {}
end

function TestBlockers:test_item_blocking_relationship()
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

  local blocked = data.get("01TEST0000000000000000001")
  local blocker = data.get("01TEST0000000000000000002")

  lu.assertEquals(blocker.blocks[1], blocked.id)
  lu.assertTrue(process.is_item_blocked(blocked))
  lu.assertFalse(process.is_item_blocked(blocker))
end

return TestBlockers
