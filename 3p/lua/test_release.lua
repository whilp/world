-- test release artifact requirements
local lu = require("luaunit")
local unix = require("cosmo.unix")
local spawn = require("spawn").spawn

local lua_bin = "results/bin/lua"

function test_lua_binary_exists()
  local st = unix.stat(lua_bin)
  lu.assertNotNil(st, "results/bin/lua should exist")
end

function test_lua_binary_is_executable()
  local st = unix.stat(lua_bin)
  lu.assertNotNil(st, "results/bin/lua should exist")
  local mode = st:mode()
  -- check user or group execute bit
  lu.assertTrue(mode % 2 == 1 or (mode / 8) % 2 == 1 or (mode / 64) % 2 == 1,
    "results/bin/lua should be executable")
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
    "spawn",
    "version",
    "platform",
  }

  for _, mod in ipairs(test_modules) do
    local script = string.format("require('%s')", mod)
    local handle = spawn({lua_bin, "-e", script})
    local ok, output, exit_code = handle:read()
    lu.assertEquals(exit_code, 0, "should require " .. mod .. ": " .. (output or ""))
  end
end

function test_lua_binary_luacheck_works()
  -- verify luacheck can run
  local handle = spawn({lua_bin, "/zip/.lua/bin/luacheck", "--version"})
  local ok, output, exit_code = handle:read()
  lu.assertEquals(exit_code, 0, "luacheck should run: " .. (output or ""))
  lu.assertStrContains(output or "", "Luacheck")
end
