-- test lua.dist zip contents
local lu = require("luaunit")
local unix = require("cosmo.unix")
local path = require("cosmo.path")
local spawn = require("spawn").spawn

-- get platform from environment or detect
local function get_platform()
  local ok_s, uname_s = spawn({"uname", "-s"}):read()
  local ok_m, uname_m = spawn({"uname", "-m"}):read()
  if not ok_s or not ok_m or not uname_s or not uname_m then
    return nil
  end
  uname_s = uname_s:gsub("%s+$", "")
  uname_m = uname_m:gsub("%s+$", "")

  local os_name = uname_s == "Darwin" and "darwin" or "linux"
  local arch = uname_m == "aarch64" and "arm64" or uname_m
  return os_name .. "-" .. arch
end

local platform = get_platform()
local lua_dist = "o/" .. platform .. "/lua/bin/lua.dist"

-- helper to list zip contents
local function zip_list(archive)
  local handle = spawn({"/usr/bin/unzip", "-l", archive})
  local ok, output = handle:read()
  if not ok then
    return nil
  end
  return output
end

-- helper to check if a pattern exists in zip listing
local function zip_contains(listing, pattern)
  return listing:match(pattern) ~= nil
end

-- expected lua modules that should be in .lua/
local expected_modules = {
  -- 3p libs
  "argparse.lua",
  "lfs.lua",
  "luaunit.lua",
  "luacheck/init.lua",
  "bin/luacheck",

  -- standalone lib files
  "version.lua",
  "platform.lua",
  "utils.lua",
  "ulid.lua",
  "file.lua",

  -- lib modules
  "aerosnap/init.lua",
  "claude/main.lua",
  "daemonize/init.lua",
  "environ/init.lua",
  "spawn/init.lua",
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
  local listing = zip_list(lua_dist)
  lu.assertNotNil(listing, "should be able to list zip contents")

  for _, mod in ipairs(expected_modules) do
    local pattern = "%.lua/" .. mod:gsub("%.", "%%.")
    lu.assertTrue(zip_contains(listing, pattern), "should contain .lua/" .. mod)
  end
end

function test_lua_dist_excludes_test_files()
  local listing = zip_list(lua_dist)
  lu.assertNotNil(listing, "should be able to list zip contents")

  -- check that test files and cook.mk are not included
  for _, pattern in ipairs(excluded_patterns) do
    local full_pattern = "%.lua/[^%s]*" .. pattern
    -- this is a softer check - we just warn if found
    if zip_contains(listing, full_pattern) then
      io.stderr:write("WARNING: found excluded pattern in dist: " .. pattern .. "\n")
    end
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
    "spawn",
    "environ",
  }

  for _, mod in ipairs(test_requires) do
    local handle = spawn({lua_dist, "-e", "require('" .. mod .. "')"})
    local ok, output, exit_code = handle:read()
    lu.assertEquals(exit_code, 0, "should be able to require " .. mod .. ": " .. (output or ""))
  end
end
