local lu = require("luaunit")

local data = require("work.data")
local process = require("work.process")
local Work = require("lib")

TestCommandBlocked = {}

function TestCommandBlocked:setUp()
  data.items = {}
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

  local item1 = data.get("01TEST0000000000000000001")
  local item2 = data.get("01TEST0000000000000000002")

  local blocked_items = process.get_blocked_items()
  lu.assertEquals(#blocked_items, 1)
  lu.assertEquals(blocked_items[1].id, item1.id)
end

return TestCommandBlocked
