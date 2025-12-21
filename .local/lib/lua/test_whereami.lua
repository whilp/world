-- test whereami module
local cosmo = require('cosmo')
local path = cosmo.path

package.path = path.join(os.getenv("HOME"), ".local", "lib", "lua", "?.lua") .. ";" .. package.path

local whereami = require('whereami')

function test_whereami_get()
  local identifier = whereami.get()
  lu.assertNotNil(identifier, "whereami.get() should return a value")
  lu.assertIsString(identifier, "whereami.get() should return a string")
  lu.assertTrue(#identifier > 0, "whereami.get() should return a non-empty string")
  lu.assertNotEquals(identifier, "unknown", "whereami.get() should not return 'unknown' in a normal environment")
end

function test_whereami_get_with_emoji()
  local identifier = whereami.get_with_emoji()
  lu.assertNotNil(identifier, "whereami.get_with_emoji() should return a value")
  lu.assertIsString(identifier, "whereami.get_with_emoji() should return a string")
  lu.assertTrue(#identifier > 0, "whereami.get_with_emoji() should return a non-empty string")
  -- Should contain a space (separating identifier and emoji)
  lu.assertNotNil(identifier:find(" "), "whereami.get_with_emoji() should contain identifier and emoji separated by space")
end
