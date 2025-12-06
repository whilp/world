local lu = require("luaunit")

local data = require("work.data")
local process = require("work.process")
local Work = require("lib")

TestOrphanedBlocks = {}

function TestOrphanedBlocks:setUp()
  data.items = {}
end

function TestOrphanedBlocks:test_find_items_blocking_on()
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

  local referencing = process.find_items_blocking_on("01TEST0000000000000000001")
  lu.assertEquals(#referencing, 2)

  local ids = {}
  for _, item in ipairs(referencing) do
    table.insert(ids, item.id)
  end
  table.sort(ids)

  lu.assertEquals(ids[1], "01TEST0000000000000000002")
  lu.assertEquals(ids[2], "01TEST0000000000000000003")
end

function TestOrphanedBlocks:test_find_items_blocking_on_no_references()
  Work{
    id = "01TEST0000000000000000001",
    title = "deploy to prod",
    created = "2025-12-01",
  }

  Work{
    id = "01TEST0000000000000000002",
    title = "implement auth",
    created = "2025-12-01",
  }

  local referencing = process.find_items_blocking_on("01TEST0000000000000000001")
  lu.assertEquals(#referencing, 0)
end

function TestOrphanedBlocks:test_find_items_blocking_on_nonexistent()
  Work{
    id = "01TEST0000000000000000001",
    title = "deploy to prod",
    created = "2025-12-01",
  }

  local referencing = process.find_items_blocking_on("01TEST9999999999999999999")
  lu.assertEquals(#referencing, 0)
end

function TestOrphanedBlocks:test_find_items_blocking_on_multiple_blocks()
  Work{
    id = "01TEST0000000000000000001",
    title = "feature A",
    created = "2025-12-01",
  }

  Work{
    id = "01TEST0000000000000000002",
    title = "feature B",
    created = "2025-12-01",
  }

  Work{
    id = "01TEST0000000000000000003",
    title = "deploy all features",
    created = "2025-12-01",
    blocks = { "01TEST0000000000000000001", "01TEST0000000000000000002" },
  }

  local referencing_a = process.find_items_blocking_on("01TEST0000000000000000001")
  lu.assertEquals(#referencing_a, 1)
  lu.assertEquals(referencing_a[1].id, "01TEST0000000000000000003")

  local referencing_b = process.find_items_blocking_on("01TEST0000000000000000002")
  lu.assertEquals(#referencing_b, 1)
  lu.assertEquals(referencing_b[1].id, "01TEST0000000000000000003")
end

return TestOrphanedBlocks
