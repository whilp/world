#!/usr/bin/env run-test.lua

local spawn = require("cosmic.child").spawn
local path = require("cosmo.path")

global TEST_DIR: string

local bin = path.join(TEST_DIR, "bin", "biome")

local function test_version()
  local ok, got = spawn({ bin, "--version" }):read()
  assert(ok, "SKIP biome not executable on this platform")
  local got_str = got as string
  assert(got_str:match("Version:") or got_str:match("%d+%.%d+%.%d+"), "checking biome version, got: " .. got_str)
end
test_version()
