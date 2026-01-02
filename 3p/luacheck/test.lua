local lu = require("luaunit")
local spawn = require("cosmic.spawn").spawn
local path = require("cosmo.path")

local bin = path.join(os.getenv("TEST_BIN_DIR"), "bin", "luacheck")

TestLuacheck = {}

function TestLuacheck:test_version()
  local handle = spawn({ bin, "--version" })
  lu.assertEquals(handle:wait(), 0)
end
