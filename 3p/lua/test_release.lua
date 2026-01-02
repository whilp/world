-- test release artifact requirements
local lu = require("luaunit")
local unix = require("cosmo.unix")
local spawn = require("cosmic.spawn")
local path = require("cosmo.path")

local lua_bin = path.join(os.getenv("TEST_BIN_DIR"), "bin", "lua.dist")

function test_lua_binary_exists()
  local st = unix.stat(lua_bin)
  lu.assertNotNil(st, lua_bin .. " should exist")
end

function test_lua_binary_runs()
  local handle = spawn({lua_bin, "-e", "print('hello')"})
  local ok, output, exit_code = handle:read()
  lu.assertTrue(ok, "lua should run successfully: exit=" .. tostring(exit_code))
  lu.assertStrContains(output or "", "hello")
end

function test_lua_binary_has_bundled_modules()
  -- test that key modules can be required
  -- the test runner unveils things so LUA_PATH from env won't interfere
  local test_modules = {
    "argparse",
    "luacheck",
    "luaunit",
    "lfs",
    "cosmic.spawn",
    "version",
    "platform",
  }

  for _, mod in ipairs(test_modules) do
    local script = string.format("require('%s')", mod)
    local handle = spawn({lua_bin, "-e", script})
    local _, output, exit_code = handle:read()
    lu.assertEquals(exit_code, 0, "should require " .. mod .. ": " .. (output or ""))
  end
end

