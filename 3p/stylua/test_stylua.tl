#!/usr/bin/env run-test.lua

global TEST_DIR: string

local spawn = require("cosmic.child").spawn
local path = require("cosmo.path")

local bin: string = path.join(TEST_DIR, "stylua")

local function test_version()
  local ok, got = spawn({ bin, "--version" }):read()
  assert(ok, "stylua --version failed")
  local want = "stylua"
  assert((got as string):find(want, 1, true), "checking stylua, want: " .. want .. " got: " .. (got as string))
end
test_version()
