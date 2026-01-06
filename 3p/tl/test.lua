local luaunit = require("luaunit")
-- teal ignore: type annotations needed
local spawn = require("cosmic.spawn")
local path = require("cosmo.path")

local lua_dist = path.join(os.getenv("TEST_BIN_DIR"):gsub("/tl$", "/lua"), "bin", "lua.dist")
local tl_bin = path.join(os.getenv("TEST_BIN_DIR"), "bin", "tl")

TestTeal = {}

function TestTeal:test_version()
  local handle = spawn({ lua_dist, tl_bin, "--version" })
  luaunit.assertEquals(handle:wait(), 0)
end

os.exit(luaunit.LuaUnit.run())
