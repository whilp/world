#!/usr/bin/env run-test.lua

local environ = require("environ")

local function test_new_empty()
  local env = environ.new()
  assert(env ~= nil)
  local arr = env:toarray()
  assert(#arr == 0, "empty environ should produce empty array")
end
test_new_empty()

local function test_new_from_array()
  local input = {"HOME=/home/user", "PATH=/usr/bin", "SHELL=/bin/bash"}
  local env = environ.new(input)

  assert(env.HOME == "/home/user")
  assert(env.PATH == "/usr/bin")
  assert(env.SHELL == "/bin/bash")
end
test_new_from_array()

local function test_get_nonexistent()
  local env = environ.new({"FOO=bar"})
  assert(env.NONEXISTENT == nil)
end
test_get_nonexistent()

local function test_set_new_variable()
  local env = environ.new()
  env.FOO = "bar"
  assert(env.FOO == "bar")
end
test_set_new_variable()

local function test_set_existing_variable()
  local env = environ.new({"FOO=original"})
  assert(env.FOO == "original")

  env.FOO = "updated"
  assert(env.FOO == "updated")
end
test_set_existing_variable()

local function test_toarray_empty()
  local env = environ.new()
  local arr = env:toarray()
  assert(arr ~= nil)
  assert(type(arr) == "table")
  assert(#arr == 0)
end
test_toarray_empty()

local function test_toarray_single_variable()
  local env = environ.new()
  env.FOO = "bar"

  local arr = env:toarray()
  assert(#arr == 1)
  assert(arr[1] == "FOO=bar")
end
test_toarray_single_variable()

local function test_toarray_multiple_variables()
  local env = environ.new()
  env.FOO = "bar"
  env.BAZ = "qux"

  local arr = env:toarray()
  assert(#arr == 2)

  local found_foo = false
  local found_baz = false
  for _, entry in ipairs(arr) do
    if entry == "FOO=bar" then found_foo = true end
    if entry == "BAZ=qux" then found_baz = true end
  end

  assert(found_foo, "should contain FOO=bar")
  assert(found_baz, "should contain BAZ=qux")
end
test_toarray_multiple_variables()

local function test_toarray_preserves_values()
  local input = {"HOME=/home/user", "PATH=/usr/bin:/bin", "SHELL=/bin/bash"}
  local env = environ.new(input)

  local arr = env:toarray()
  assert(#arr == 3)

  local found: {string:boolean} = {}
  for _, entry in ipairs(arr) do
    found[entry] = true
  end

  assert(found["HOME=/home/user"], "should preserve HOME")
  assert(found["PATH=/usr/bin:/bin"], "should preserve PATH")
  assert(found["SHELL=/bin/bash"], "should preserve SHELL")
end
test_toarray_preserves_values()

local function test_parse_with_equals_in_value()
  local input = {"URL=https://example.com?foo=bar"}
  local env = environ.new(input)

  assert(env.URL == "https://example.com?foo=bar")
end
test_parse_with_equals_in_value()

local function test_parse_empty_value()
  local input = {"EMPTY="}
  local env = environ.new(input)

  assert(env.EMPTY == "")
end
test_parse_empty_value()

local function test_replace_variable()
  local input = {"GH_HOST=git.corp.stripe.com", "PATH=/usr/bin"}
  local env = environ.new(input)

  assert(env.GH_HOST == "git.corp.stripe.com")

  env.GH_HOST = "github.com"
  assert(env.GH_HOST == "github.com")

  local arr = env:toarray()
  assert(#arr == 2)

  local found_gh = false
  local found_path = false
  for _, entry in ipairs(arr) do
    if entry == "GH_HOST=github.com" then found_gh = true end
    if entry == "PATH=/usr/bin" then found_path = true end
  end

  assert(found_gh, "should contain updated GH_HOST")
  assert(found_path, "should preserve PATH")
end
test_replace_variable()

local function test_roundtrip()
  local input = {"FOO=bar", "BAZ=qux", "NUM=123"}
  local env1 = environ.new(input)

  local arr = env1:toarray()
  local env2 = environ.new(arr)

  assert(env2.FOO == "bar")
  assert(env2.BAZ == "qux")
  assert(env2.NUM == "123")
end
test_roundtrip()
