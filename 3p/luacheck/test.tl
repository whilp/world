local lu = require("luaunit")
local spawn = require("cosmic.spawn")
local path = require("cosmo.path")

local record SpawnHandle
  wait: function(self: SpawnHandle): number
end

global TEST_BIN_DIR: string

local bin = path.join(os.getenv("TEST_BIN_DIR") as string, "bin", "luacheck")

global TestLuacheck: {string: function()}
TestLuacheck = {}

function TestLuacheck.test_version()
  local handle = spawn({ bin, "--version" }) as SpawnHandle
  lu.assertEquals(handle:wait(), 0)
end
