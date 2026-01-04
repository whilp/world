#!/usr/bin/env run-test.lua

local lu = require("luaunit")
local env = require("cosmic.env")

TestEnv = {}

function TestEnv:test_get_existing()
  local arr = {"HOME=/home/user", "PATH=/usr/bin"}
  lu.assertEquals(env.get(arr, "HOME"), "/home/user")
  lu.assertEquals(env.get(arr, "PATH"), "/usr/bin")
end

function TestEnv:test_get_nonexistent()
  local arr = {"HOME=/home/user"}
  lu.assertNil(env.get(arr, "NONEXISTENT"))
end

function TestEnv:test_get_empty_array()
  local arr = {}
  lu.assertNil(env.get(arr, "PATH"))
end

function TestEnv:test_get_with_equals_in_value()
  local arr = {"URL=https://example.com?foo=bar"}
  lu.assertEquals(env.get(arr, "URL"), "https://example.com?foo=bar")
end

function TestEnv:test_get_empty_value()
  local arr = {"EMPTY="}
  lu.assertEquals(env.get(arr, "EMPTY"), "")
end

function TestEnv:test_set_new()
  local arr = {"HOME=/home/user"}
  env.set(arr, "PATH", "/usr/bin")
  lu.assertEquals(#arr, 2)
  lu.assertEquals(env.get(arr, "PATH"), "/usr/bin")
  lu.assertEquals(env.get(arr, "HOME"), "/home/user")
end

function TestEnv:test_set_update_existing()
  local arr = {"PATH=/usr/bin", "HOME=/home/user"}
  env.set(arr, "PATH", "/usr/local/bin")
  lu.assertEquals(#arr, 2)
  lu.assertEquals(env.get(arr, "PATH"), "/usr/local/bin")
end

function TestEnv:test_set_empty_array()
  local arr = {}
  env.set(arr, "FOO", "bar")
  lu.assertEquals(#arr, 1)
  lu.assertEquals(arr[1], "FOO=bar")
end

function TestEnv:test_unset_existing()
  local arr = {"FOO=bar", "BAZ=qux"}
  local removed = env.unset(arr, "FOO")
  lu.assertTrue(removed)
  lu.assertEquals(#arr, 1)
  lu.assertEquals(arr[1], "BAZ=qux")
  lu.assertNil(env.get(arr, "FOO"))
end

function TestEnv:test_unset_nonexistent()
  local arr = {"FOO=bar"}
  local removed = env.unset(arr, "NONEXISTENT")
  lu.assertFalse(removed)
  lu.assertEquals(#arr, 1)
end

function TestEnv:test_prepend_path_existing()
  local arr = {"PATH=/usr/bin:/bin"}
  env.prepend_path(arr, "/usr/local/bin")
  lu.assertEquals(env.get(arr, "PATH"), "/usr/local/bin:/usr/bin:/bin")
end

function TestEnv:test_prepend_path_empty()
  local arr = {"HOME=/home/user"}
  env.prepend_path(arr, "/usr/local/bin")
  lu.assertEquals(env.get(arr, "PATH"), "/usr/local/bin")
end

function TestEnv:test_prepend_path_empty_value()
  local arr = {"PATH="}
  env.prepend_path(arr, "/usr/local/bin")
  lu.assertEquals(env.get(arr, "PATH"), "/usr/local/bin")
end

function TestEnv:test_append_path_existing()
  local arr = {"PATH=/usr/bin:/bin"}
  env.append_path(arr, "/usr/local/bin")
  lu.assertEquals(env.get(arr, "PATH"), "/usr/bin:/bin:/usr/local/bin")
end

function TestEnv:test_append_path_empty()
  local arr = {"HOME=/home/user"}
  env.append_path(arr, "/usr/local/bin")
  lu.assertEquals(env.get(arr, "PATH"), "/usr/local/bin")
end

function TestEnv:test_multiple_operations()
  local arr = {}
  env.set(arr, "HOME", "/home/user")
  env.set(arr, "PATH", "/usr/bin")
  env.prepend_path(arr, "/usr/local/bin")
  env.set(arr, "CC", "clang")

  lu.assertEquals(env.get(arr, "HOME"), "/home/user")
  lu.assertEquals(env.get(arr, "PATH"), "/usr/local/bin:/usr/bin")
  lu.assertEquals(env.get(arr, "CC"), "clang")
  lu.assertEquals(#arr, 3)
end
