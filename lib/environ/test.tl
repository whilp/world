local lu = require("luaunit")

local environ = require("environ")

function test_new_empty()
  local env = environ.new()
  lu.assertNotNil(env)
  local arr = env:toarray()
  lu.assertEquals(#arr, 0, "empty environ should produce empty array")
end

function test_new_from_array()
  local input = {"HOME=/home/user", "PATH=/usr/bin", "SHELL=/bin/bash"}
  local env = environ.new(input)

  lu.assertEquals(env.HOME, "/home/user")
  lu.assertEquals(env.PATH, "/usr/bin")
  lu.assertEquals(env.SHELL, "/bin/bash")
end

function test_get_nonexistent()
  local env = environ.new({"FOO=bar"})
  lu.assertNil(env.NONEXISTENT)
end

function test_set_new_variable()
  local env = environ.new()
  env.FOO = "bar"
  lu.assertEquals(env.FOO, "bar")
end

function test_set_existing_variable()
  local env = environ.new({"FOO=original"})
  lu.assertEquals(env.FOO, "original")

  env.FOO = "updated"
  lu.assertEquals(env.FOO, "updated")
end

function test_toarray_empty()
  local env = environ.new()
  local arr = env:toarray()
  lu.assertNotNil(arr)
  lu.assertEquals(type(arr), "table")
  lu.assertEquals(#arr, 0)
end

function test_toarray_single_variable()
  local env = environ.new()
  env.FOO = "bar"

  local arr = env:toarray()
  lu.assertEquals(#arr, 1)
  lu.assertEquals(arr[1], "FOO=bar")
end

function test_toarray_multiple_variables()
  local env = environ.new()
  env.FOO = "bar"
  env.BAZ = "qux"

  local arr = env:toarray()
  lu.assertEquals(#arr, 2)

  local found_foo = false
  local found_baz = false
  for _, entry in ipairs(arr) do
    if entry == "FOO=bar" then found_foo = true end
    if entry == "BAZ=qux" then found_baz = true end
  end

  lu.assertTrue(found_foo, "should contain FOO=bar")
  lu.assertTrue(found_baz, "should contain BAZ=qux")
end

function test_toarray_preserves_values()
  local input = {"HOME=/home/user", "PATH=/usr/bin:/bin", "SHELL=/bin/bash"}
  local env = environ.new(input)

  local arr = env:toarray()
  lu.assertEquals(#arr, 3)

  local found: {string:boolean} = {}
  for _, entry in ipairs(arr) do
    found[entry] = true
  end

  lu.assertTrue(found["HOME=/home/user"], "should preserve HOME")
  lu.assertTrue(found["PATH=/usr/bin:/bin"], "should preserve PATH")
  lu.assertTrue(found["SHELL=/bin/bash"], "should preserve SHELL")
end

function test_parse_with_equals_in_value()
  local input = {"URL=https://example.com?foo=bar"}
  local env = environ.new(input)

  lu.assertEquals(env.URL, "https://example.com?foo=bar")
end

function test_parse_empty_value()
  local input = {"EMPTY="}
  local env = environ.new(input)

  lu.assertEquals(env.EMPTY, "")
end

function test_replace_variable()
  local input = {"GH_HOST=git.corp.stripe.com", "PATH=/usr/bin"}
  local env = environ.new(input)

  lu.assertEquals(env.GH_HOST, "git.corp.stripe.com")

  env.GH_HOST = "github.com"
  lu.assertEquals(env.GH_HOST, "github.com")

  local arr = env:toarray()
  lu.assertEquals(#arr, 2)

  local found_gh = false
  local found_path = false
  for _, entry in ipairs(arr) do
    if entry == "GH_HOST=github.com" then found_gh = true end
    if entry == "PATH=/usr/bin" then found_path = true end
  end

  lu.assertTrue(found_gh, "should contain updated GH_HOST")
  lu.assertTrue(found_path, "should preserve PATH")
end

function test_roundtrip()
  local input = {"FOO=bar", "BAZ=qux", "NUM=123"}
  local env1 = environ.new(input)

  local arr = env1:toarray()
  local env2 = environ.new(arr)

  lu.assertEquals(env2.FOO, "bar")
  lu.assertEquals(env2.BAZ, "qux")
  lu.assertEquals(env2.NUM, "123")
end
