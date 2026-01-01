local lu = require("luaunit")
local spawn = require("spawn").spawn
local path = require("cosmo.path")
local unix = require("cosmo.unix")

local test_bin_dir = os.getenv("TEST_BIN_DIR")
local project_root = path.join(path.dirname(test_bin_dir), "..", "..")
local lua_bin = path.join(project_root, "o", "any", "lua", "bin", "lua")
local test_dir = path.join(project_root, "3p", "luacheck", "test_files")
local config_path = path.join(project_root, ".luacheckrc")
local luacheck_lib = path.join(os.getenv("TEST_BIN_DIR"), "lib")
local argparse_lib = path.join(project_root, "o", "any", "argparse", "lib")
local lfs_lib = path.join(project_root, "o", "any", "lfs", "lib")

local function write_test_file(filename, content)
  unix.makedirs(test_dir, tonumber("755", 8))
  local filepath = path.join(test_dir, filename)
  local fd = unix.open(filepath, unix.O_WRONLY | unix.O_CREAT | unix.O_TRUNC, tonumber("644", 8))
  unix.write(fd, content)
  unix.close(fd)
  return filepath
end

local function run_luacheck(filepath)
  local lua_path = string.format(
    "%s/?.lua;%s/?/init.lua;%s/?.lua;%s/?/init.lua;%s/?.lua;%s/?/init.lua",
    luacheck_lib, luacheck_lib,
    argparse_lib, argparse_lib,
    lfs_lib, lfs_lib
  )
  local code = string.format(
    'package.path = %q .. ";" .. package.path; arg = {[0] = "luacheck", "--config", %q, %q}; require("luacheck.main")',
    lua_path, config_path, filepath
  )
  local handle = spawn({ lua_bin, "-e", code })
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
  local code = [[
local very_long_line = "this is an extremely long line that definitely exceeds the maximum line length of 120 characters"
return very_long_line
]]
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
