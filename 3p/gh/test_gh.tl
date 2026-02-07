#!/usr/bin/env run-test.lua

local spawn = require("cosmic.child").spawn
local path = require("cosmo.path")

global TEST_DIR: string

local bin = path.join(TEST_DIR, "bin", "gh")

local function test_version()
  local ok, got = spawn({ bin, "--version" }):read()
  assert(ok, "gh --version failed")
  local want = "gh version"
  assert((got as string):find(want, 1, true), "checking gh, want: " .. want .. " got: " .. (got as string))
end
test_version()
