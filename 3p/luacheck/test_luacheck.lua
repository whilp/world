#!/usr/bin/env run-test.lua
-- teal ignore: test file
-- ast-grep ignore: test file needs package.path manipulation

local path = require("cosmo.path")

-- add staged src to package.path
package.path = path.join(TEST_DIR, "src", "?.lua") .. ";"
  .. path.join(TEST_DIR, "src", "?", "init.lua") .. ";"
  .. package.path

local luacheck = require("luacheck")

local function test_load_library()
  assert(luacheck, "failed to load luacheck")
  assert(type(luacheck.check_strings) == "function", "expected check_strings function")
end
test_load_library()

local function test_check_string()
  local reports = luacheck.check_strings({"local x = 1"})
  assert(#reports == 1, "expected one report")
  -- reports[1] is an array of warnings/errors
  assert(type(reports[1]) == "table", "expected table")
  assert(#reports[1] >= 1, "expected at least one warning for unused variable")
end
test_check_string()
