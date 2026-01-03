#!/usr/bin/env run-test.lua

local spawn = require("cosmic.spawn")
local path = require("cosmo.path")

local bin = path.join(TEST_DIR, "marksman")

local function test_version()
  local ok, got = spawn({ bin, "--version" }):read()
  if not ok then skip("binary not executable on this platform") end
  -- version output is just version number like "1.0.0-8299f11+..."
  assert(got:match("%d+%.%d+%.%d+"), "checking marksman version, got: " .. got)
end
test_version()
