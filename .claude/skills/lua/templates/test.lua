-- Set up package path to find library modules
local script_path = debug.getinfo(1, "S").source:sub(2)
local script_dir = script_path:match("(.+)/[^/]+$")
if script_dir then
  package.path = script_dir .. "/../../.local/lib/lua/?.lua;" .. package.path
else
  package.path = "../../.local/lib/lua/?.lua;" .. package.path
end

local cosmo = require('cosmo')
local unix = cosmo.unix

-- Set up package path to find the module under test
if script_dir then
  package.path = script_dir .. "/../?.lua;" .. package.path
else
  package.path = "../?.lua;" .. package.path
end

-- Import the module to test (replace 'mymodule' with your module name)
local mymodule = require("mymodule.main")

-- Test success case
function test_function_returns_expected()
  local result = mymodule.helper_function("input")

  lu.assertTrue(result ~= nil, "should not return nil")
  lu.assertTrue(type(result) == "boolean", "should return boolean")
end

-- Test error case
function test_function_handles_empty_input()
  local result, err = mymodule.helper_function("")

  lu.assertNil(result, "should return nil on error")
  lu.assertTrue(type(err) == "string", "should return error message")
  lu.assertStrContains(err, "empty", "error should mention empty")
end

-- Test with valid data
function test_main_returns_zero_on_success()
  local exit_code = mymodule.main({"help"})

  lu.assertEquals(exit_code, 0, "should return 0 on success")
end

-- Test with invalid command
function test_main_returns_nonzero_on_error()
  local exit_code = mymodule.main({"invalid"})

  lu.assertTrue(exit_code ~= 0, "should return non-zero on error")
end
