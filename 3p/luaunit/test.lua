local lu = require("luaunit")

TestLuaunit = {}

function TestLuaunit:test_assert_equals()
  lu.assertEquals(1 + 1, 2)
end

function TestLuaunit:test_assert_not_nil()
  lu.assertNotNil(lu)
end
