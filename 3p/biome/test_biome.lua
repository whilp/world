#!/usr/bin/env run-test.lua

local spawn = require("cosmic.spawn")
local path = require("cosmo.path")

local bin = path.join(TEST_DIR, "biome")

local function test_version()
  local ok, got = spawn({ bin, "--version" }):read()
  assert(ok, "SKIP biome not executable on this platform")
  assert(got:match("Version:") or got:match("%d+%.%d+%.%d+"), "checking biome version, got: " .. got)
end
test_version()
