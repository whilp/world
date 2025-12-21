-- Note: Package paths are configured in src/test.mk via LUA_PATH
-- Add your test target there with paths to .local/lib/lua and src modules

local cosmo = require('cosmo')
local unix = cosmo.unix
local path = cosmo.path

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
