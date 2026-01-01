local lu = require("luaunit")
local cosmo = require("cosmo")
local spawn = require("spawn").spawn
local path = require("cosmo.path")

local test_bin_dir = os.getenv("TEST_BIN_DIR")
local luacheck_bin = path.join(test_bin_dir, "bin", "luacheck")
local config_path = TEST_ARGS[1]
if not config_path then
  error("missing config path in TEST_ARGS[1]")
end

local function run_luacheck(filepath)
  -- Use project config, override include_files to check just this file
  -- Use -- to separate options from file arguments
  local handle = spawn({
    luacheck_bin, "--config", config_path, "--include-files", filepath, "--", filepath
  })
  local status = handle:wait()
  return status
end

TestLuacheckRules = {}

function TestLuacheckRules:test_undefined_global_detected()
  local code = [[
local x = 10
y = 20
return x + y
]]
  local filepath = path.join(TEST_TMPDIR, "check_global_bad.lua")
  cosmo.Barf(filepath, code)
  local status = run_luacheck(filepath)
  lu.assertNotEquals(status, 0, "should detect undefined global variable")
end

function TestLuacheckRules:test_clean_code_passes()
  local code = [[
local x = 10
return x
]]
  local filepath = path.join(TEST_TMPDIR, "check_clean.lua")
  cosmo.Barf(filepath, code)
  local status = run_luacheck(filepath)
  lu.assertEquals(status, 0, "should pass for clean code")
end

function TestLuacheckRules:test_unused_variable_detected()
  local code = [[
local x = 10
local y = 20
return y
]]
  local filepath = path.join(TEST_TMPDIR, "check_unused_bad.lua")
  cosmo.Barf(filepath, code)
  local status = run_luacheck(filepath)
  lu.assertNotEquals(status, 0, "should detect unused variable")
end

function TestLuacheckRules:test_all_variables_used_passes()
  local code = [[
local x = 10
local y = 20
return x + y
]]
  local filepath = path.join(TEST_TMPDIR, "check_used_good.lua")
  cosmo.Barf(filepath, code)
  local status = run_luacheck(filepath)
  lu.assertEquals(status, 0, "should pass when all variables are used")
end

function TestLuacheckRules:test_variable_shadowing_detected()
  local code = [[
local x = 10
local function foo()
  local x = 20
  return x
end
return foo() + x
]]
  local filepath = path.join(TEST_TMPDIR, "check_shadow_bad.lua")
  cosmo.Barf(filepath, code)
  local status = run_luacheck(filepath)
  lu.assertNotEquals(status, 0, "should detect variable shadowing")
end

function TestLuacheckRules:test_no_shadowing_passes()
  local code = [[
local x = 10
local function foo(y)
  return y
end
return foo(x)
]]
  local filepath = path.join(TEST_TMPDIR, "check_shadow_good.lua")
  cosmo.Barf(filepath, code)
  local status = run_luacheck(filepath)
  lu.assertEquals(status, 0, "should pass when no shadowing")
end

function TestLuacheckRules:test_line_too_long_detected()
  -- Build a line >120 chars without exceeding 120 chars in this file
  local long_str = string.rep("x", 130)
  local code = 'local v = "' .. long_str .. '"\nreturn v\n'
  local filepath = path.join(TEST_TMPDIR, "check_long_line_bad.lua")
  cosmo.Barf(filepath, code)
  local status = run_luacheck(filepath)
  lu.assertNotEquals(status, 0, "should detect line exceeding max length")
end

function TestLuacheckRules:test_short_line_passes()
  local code = [[
local short = "short"
return short
]]
  local filepath = path.join(TEST_TMPDIR, "check_short_line.lua")
  cosmo.Barf(filepath, code)
  local status = run_luacheck(filepath)
  lu.assertEquals(status, 0, "should pass for short lines")
end
