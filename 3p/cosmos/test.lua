local lu = require("luaunit")
local spawn = require("spawn")
local path = require("cosmo.path")

local bin = path.join(os.getenv("TEST_BIN_DIR"), "bin", "lua")

TestCosmos = {}

function TestCosmos:test_version()
  local handle = spawn({ bin, "-v" })
  lu.assertEquals(handle:wait(), 0)
end
