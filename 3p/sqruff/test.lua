local lu = require("luaunit")
local spawn = require("cosmic.spawn").spawn
local path = require("cosmo.path")

local bin = path.join(os.getenv("TEST_BIN_DIR"), "bin", "sqruff")

TestSqruff = {}

function TestSqruff:test_version()
  local handle = spawn({ bin, "--version" })
  lu.assertEquals(handle:wait(), 0)
end
