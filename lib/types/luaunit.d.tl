-- type declarations for luaunit

local record luaunit
  -- equality assertions
  assertEquals: function(actual: any, expected: any, msg?: string)
  assertNotEquals: function(actual: any, expected: any, msg?: string)

  -- nil assertions
  assertNil: function(value: any, msg?: string)
  assertNotNil: function(value: any, msg?: string)

  -- boolean assertions
  assertTrue: function(value: any, msg?: string)
  assertFalse: function(value: any, msg?: string)

  -- string assertions
  assertStrContains: function(str: string, substring: string, msg?: string)
  assertNotStrContains: function(str: string, substring: string, msg?: string)
  assertStrMatches: function(str: string, pattern: string, msg?: string)

  -- type assertions
  assertIsString: function(value: any, msg?: string)
  assertIsNumber: function(value: any, msg?: string)
  assertIsTable: function(value: any, msg?: string)
  assertIsFunction: function(value: any, msg?: string)
  assertIsBoolean: function(value: any, msg?: string)

  -- error assertions
  assertError: function(fn: function, ...: any)
  assertErrorMsgContains: function(expected: string, fn: function, ...: any)

  -- table assertions
  assertItemsEquals: function(actual: {any}, expected: {any}, msg?: string)

  -- skip test
  skip: function(msg: string)

  -- run tests
  run: function(args?: {string}): number
end

return luaunit
