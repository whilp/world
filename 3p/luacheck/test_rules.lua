local lu = require("luaunit")
local spawn = require("spawn").spawn
local path = require("cosmo.path")
local unix = require("cosmo.unix")

local test_bin_dir = os.getenv("TEST_BIN_DIR")
local luacheck_bin = path.join(test_bin_dir, "bin", "luacheck")
local test_dir = path.join(TEST_TMPDIR, "luacheck_test_files")

local function write_test_file(filename, content)
  unix.makedirs(test_dir, tonumber("755", 8))
  local filepath = path.join(test_dir, filename)
  local fd = unix.open(filepath, unix.O_WRONLY | unix.O_CREAT | unix.O_TRUNC, tonumber("644", 8))
  unix.write(fd, content)
  unix.close(fd)
  return filepath
end

local function run_luacheck(filepath)
  -- Use --no-config to prevent loading .luacheckrc which has include_files restrictions
  -- Also use --std lua54 for consistency with the project's lua version
  local handle = spawn({ luacheck_bin, "--no-config", "--std", "lua54", filepath })
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
  local filepath = write_test_file("check_global_bad.lua", code)
  local status = run_luacheck(filepath)
  lu.assertNotEquals(status, 0, "should detect undefined global variable")
  unix.unlink(filepath)
end

function TestLuacheckRules:test_clean_code_passes()
  local code = [[
local x = 10
return x
]]
  local filepath = write_test_file("check_clean.lua", code)
  local status = run_luacheck(filepath)
  lu.assertEquals(status, 0, "should pass for clean code")
  unix.unlink(filepath)
end

function TestLuacheckRules:test_unused_variable_detected()
  local code = [[
local x = 10
local y = 20
return y
]]
  local filepath = write_test_file("check_unused_bad.lua", code)
  local status = run_luacheck(filepath)
  lu.assertNotEquals(status, 0, "should detect unused variable")
  unix.unlink(filepath)
end

function TestLuacheckRules:test_all_variables_used_passes()
  local code = [[
local x = 10
local y = 20
return x + y
]]
  local filepath = write_test_file("check_used_good.lua", code)
  local status = run_luacheck(filepath)
  lu.assertEquals(status, 0, "should pass when all variables are used")
  unix.unlink(filepath)
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
  local filepath = write_test_file("check_shadow_bad.lua", code)
  local status = run_luacheck(filepath)
  lu.assertNotEquals(status, 0, "should detect variable shadowing")
  unix.unlink(filepath)
end

function TestLuacheckRules:test_no_shadowing_passes()
  local code = [[
local x = 10
local function foo(y)
  return y
end
return foo(x)
]]
  local filepath = write_test_file("check_shadow_good.lua", code)
  local status = run_luacheck(filepath)
  lu.assertEquals(status, 0, "should pass when no shadowing")
  unix.unlink(filepath)
end

function TestLuacheckRules:test_line_too_long_detected()
  -- Build a line >120 chars without exceeding 120 chars in this file
  local long_str = string.rep("x", 130)
  local code = 'local v = "' .. long_str .. '"\nreturn v\n'
  local filepath = write_test_file("check_long_line_bad.lua", code)
  local status = run_luacheck(filepath)
  lu.assertNotEquals(status, 0, "should detect line exceeding max length")
  unix.unlink(filepath)
end

function TestLuacheckRules:test_short_line_passes()
  local code = [[
local short = "short"
return short
]]
  local filepath = write_test_file("check_short_line.lua", code)
  local status = run_luacheck(filepath)
  lu.assertEquals(status, 0, "should pass for short lines")
  unix.unlink(filepath)
end
