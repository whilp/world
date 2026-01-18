#!/usr/bin/env run-test.lua

local spawn = require("cosmic.spawn")
local path = require("cosmo.path")

global TEST_DIR: string

local bin = path.join(TEST_DIR, "bin", "bun")

local function test_version()
  local ok, got = spawn({ bin, "--version" }):read()
  assert(ok, "bun --version failed")
  local want = "1."
  assert((got as string):find(want, 1, true), "checking bun, want: " .. want .. " got: " .. (got as string))
end
test_version()
