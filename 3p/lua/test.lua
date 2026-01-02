-- test lua.dist zip contents
local lu = require("luaunit")
local unix = require("cosmo.unix")
local path = require("cosmo.path")
local zip = require("cosmo.zip")
local spawn = require("cosmic.spawn")

local lua_dist = path.join(os.getenv("TEST_BIN_DIR"), "bin", "lua.dist")

-- helper to check if any entry matches a pattern
local function zip_contains(entries, pattern)
  for _, entry in ipairs(entries) do
    if entry:match(pattern) then
      return true
    end
  end
  return false
end

-- expected lua modules that should be in .lua/
local expected_modules = {
  -- 3p libs
  "argparse.lua",
  "lfs.lua",
  "luaunit.lua",
  "luacheck/init.lua",

  -- standalone lib files
  "version.lua",
  "platform.lua",
  "utils.lua",
  "ulid.lua",
  "file.lua",

  -- lib modules
  "aerosnap/init.lua",
  "claude/main.lua",
  "cosmic/spawn.lua",
  "cosmic/walk.lua",
  "daemonize/init.lua",
  "environ/init.lua",
  "whereami/init.lua",
  "work/api.lua",
  "home/main.lua",
  "nvim/main.lua",
}

-- modules that should NOT be in the dist (test files, cook.mk, etc)
local excluded_patterns = {
  "cook%.mk",
  "/test_[^/]+%.lua",  -- test_*.lua files
  "/test%.lua",        -- test.lua files
  "%.ok$",             -- .ok files
}

function test_lua_dist_exists()
  local st = unix.stat(lua_dist)
  lu.assertNotNil(st, "lua.dist should exist at " .. lua_dist)
end

function test_lua_dist_is_executable()
  local st = unix.stat(lua_dist)
  lu.assertNotNil(st, "lua.dist should exist")
  local mode = st:mode()
  lu.assertTrue(mode % 2 == 1 or (mode / 64) % 2 == 1, "lua.dist should be executable")
end

function test_lua_dist_contains_expected_modules()
  local z = zip.open(lua_dist)
  lu.assertNotNil(z, "should be able to open zip")
  local entries = z:list()
  z:close()

  for _, mod in ipairs(expected_modules) do
    local pattern = "%.lua/" .. mod:gsub("%.", "%%.")
    lu.assertTrue(zip_contains(entries, pattern), "should contain .lua/" .. mod)
  end
end

function test_lua_dist_excludes_test_files()
  local z = zip.open(lua_dist)
  lu.assertNotNil(z, "should be able to open zip")
  local entries = z:list()
  z:close()

  -- check that test files and cook.mk are not included
  for _, pattern in ipairs(excluded_patterns) do
    local full_pattern = "%.lua/.*" .. pattern
    lu.assertFalse(zip_contains(entries, full_pattern),
      "should not contain excluded pattern: " .. pattern)
  end
end

function test_lua_dist_runs()
  local handle = spawn({lua_dist, "-e", "print('hello')"})
  local ok, output, exit_code = handle:read()
  lu.assertTrue(ok, "lua.dist should run successfully: exit=" .. tostring(exit_code))
  lu.assertStrContains(output or "", "hello", "should print hello")
end

function test_lua_dist_can_require_modules()
  -- test that key modules can be required
  local test_requires = {
    "argparse",
    "luaunit",
    "luacheck",
    "lfs",
    "cosmic.spawn",
    "cosmic.walk",
    "environ",
  }

  for _, mod in ipairs(test_requires) do
    local handle = spawn({lua_dist, "-e", "require('" .. mod .. "')"})
    local _, output, exit_code = handle:read()
    lu.assertEquals(exit_code, 0, "should be able to require " .. mod .. ": " .. (output or ""))
  end
end
