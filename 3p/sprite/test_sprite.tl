#!/usr/bin/env run-test.lua

local spawn = require("cosmic.child").spawn
local path = require("cosmo.path")

global TEST_DIR: string

local bin = path.join(TEST_DIR, "bin", "sprite")

local function test_version()
  local ok, got = spawn({ bin, "--version" }):read()
  assert(ok, "SKIP sprite not executable on this platform")
  local want = "0.0.1"
  assert((got as string):find(want, 1, true), "checking sprite, want: " .. want .. " got: " .. (got as string))
end
test_version()
