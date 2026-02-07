#!/usr/bin/env run-test.lua

global TEST_DIR: string

local spawn = require("cosmic.child").spawn
local path = require("cosmo.path")

local bin: string = path.join(TEST_DIR, "uv")

local function test_version()
  local ok, got = spawn({ bin, "--version" }):read()
  assert(ok, "uv --version failed")
  local want = "uv"
  assert((got as string):find(want, 1, true), "checking uv, want: " .. want .. " got: " .. (got as string))
end
test_version()
