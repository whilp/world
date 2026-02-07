#!/usr/bin/env run-test.lua

local spawn = require("cosmic.child").spawn
local path = require("cosmo.path")

global TEST_DIR: string

local bin = path.join(TEST_DIR, "bin", "comrak")

local function test_version()
  local ok, got = spawn({ bin, "--version" }):read()
  assert(ok, "SKIP comrak not executable on this platform")
  local want = "comrak"
  assert((got as string):find(want, 1, true), "checking comrak, want: " .. want .. " got: " .. (got as string))
end
test_version()
