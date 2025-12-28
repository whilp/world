local cosmo = require('cosmo')
local unix = cosmo.unix

local mymodule = require("mymodule")

function test_function_returns_expected()
  local result = mymodule.helper_function("input")

  lu.assertTrue(result ~= nil, "should not return nil")
  lu.assertEquals(result, "input", "should return input")
end

function test_function_handles_empty_input()
  local result, err = mymodule.helper_function("")

  lu.assertNil(result, "should return nil on error")
  lu.assertTrue(type(err) == "string", "should return error message")
  lu.assertStrContains(err, "empty", "error should mention empty")
end

function test_main_returns_zero_on_success()
  local exit_code = mymodule.main({"help"})

  lu.assertEquals(exit_code, 0, "should return 0 on success")
end
