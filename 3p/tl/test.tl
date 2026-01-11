local luaunit = require("luaunit")
local spawn = require("cosmic.spawn")
local path = require("cosmo.path")

local record SpawnHandle
  wait: function(self: SpawnHandle): number
end

global TEST_BIN_DIR: string

local lua_dist = path.join((os.getenv("TEST_BIN_DIR") as string):gsub("/tl$", "/lua"), "bin", "lua.dist")
local tl_bin = path.join(os.getenv("TEST_BIN_DIR") as string, "bin", "tl")

global TestTeal: {string: function()}
TestTeal = {}

function TestTeal.test_version()
  local handle = spawn({ lua_dist, tl_bin, "--version" }) as SpawnHandle
  luaunit.assertEquals(handle:wait(), 0)
end

os.exit(luaunit.LuaUnit.run())
