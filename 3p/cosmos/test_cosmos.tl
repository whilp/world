#!/usr/bin/env run-test.lua

local spawn = require("cosmic.spawn")
local path = require("cosmo.path")

global TEST_DIR: string

local function test_lua()
  local bin = path.join(TEST_DIR, "lua")
  local ok, got = spawn({ bin, "-v" }):read()
  assert(ok, "lua -v failed")
  local want = "Lua"
  assert((got as string):find(want, 1, true), "expected '" .. want .. "' in output, got: " .. (got as string))
end
test_lua()

local function test_zip()
  local bin = path.join(TEST_DIR, "zip")
  local ok, got = spawn({ bin, "--version" }):read()
  assert(ok, "zip --version failed")
  local want = "Zip"
  assert((got as string):find(want, 1, true), "expected '" .. want .. "' in output, got: " .. (got as string))
end
test_zip()
