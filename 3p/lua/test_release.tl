#!/usr/bin/env run-test.lua
--test:false
-- test release artifact requirements
local unix = require("cosmo.unix")
local spawn = require("cosmic.spawn")
local path = require("cosmo.path")

global TEST_BIN_DIR: string

local lua_bin = path.join(os.getenv("TEST_BIN_DIR") as string, "bin", "lua.dist")

local function test_lua_binary_exists()
  local st = unix.stat(lua_bin)
  assert(st ~= nil, lua_bin .. " should exist")
end
test_lua_binary_exists()

local function test_lua_binary_runs()
  local handle = spawn({lua_bin, "-e", "print('hello')"})
  local ok, output, exit_code = handle:read()
  assert(ok, "lua should run successfully: exit=" .. tostring(exit_code))
  assert((output or ""):find("hello", 1, true), "output should contain 'hello'")
end
test_lua_binary_runs()

local function test_lua_binary_has_bundled_modules()
  -- test that key modules can be required
  -- the test runner unveils things so LUA_PATH from env won't interfere
  local test_modules: {string} = {
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
    local _, output, exit_code = handle:read()
    assert(exit_code == 0, "should require " .. mod .. ": " .. (output or ""))
  end
end
test_lua_binary_has_bundled_modules()
