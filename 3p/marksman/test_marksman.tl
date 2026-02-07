#!/usr/bin/env run-test.lua

local spawn = require("cosmic.child").spawn
local path = require("cosmo.path")

global TEST_DIR: string

local bin = path.join(TEST_DIR, "bin", "marksman")

local function test_version()
  local ok, got = spawn({ bin, "--version" }):read()
  assert(ok, "SKIP marksman not executable on this platform")
  -- version output is just version number like "1.0.0-8299f11+..."
  assert((got as string):match("%d+%.%d+%.%d+"), "checking marksman version, got: " .. (got as string))
end
test_version()
