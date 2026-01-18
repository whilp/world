#!/usr/bin/env run-test.lua

global TEST_DIR: string

local spawn = require("cosmic.spawn")
local path = require("cosmo.path")

local bin: string = path.join(TEST_DIR, "sqruff")

local function test_version()
  local ok, got = spawn({ bin, "--version" }):read()
  assert(ok, "sqruff --version failed")
  local want = "sqruff"
  assert((got as string):find(want, 1, true), "checking sqruff, want: " .. want .. " got: " .. (got as string))
end
test_version()
