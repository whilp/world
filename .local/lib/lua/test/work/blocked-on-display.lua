local lu = require("luaunit")

local data = require("work.data")
local process = require("work.process")
local Work = require("lib")

TestBlockedOnDisplay = {}

function TestBlockedOnDisplay:setUp()
  data.items = {}
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

  local blocked_item = data.get("01TEST0000000000000000001")

  lu.assertTrue(process.is_item_blocked(blocked_item))

  local unresolved = process.get_unresolved_blocks(blocked_item)

  lu.assertTrue(#unresolved > 0, "blocked item should show what items are blocking it, got: " .. #unresolved)
end

return TestBlockedOnDisplay
