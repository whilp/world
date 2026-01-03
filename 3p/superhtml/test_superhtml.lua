#!/usr/bin/env run-test.lua

local spawn = require("cosmic.spawn")
local path = require("cosmo.path")

local bin = path.join(TEST_DIR, "superhtml")

local function test_version()
  local exit_code = spawn({ bin, "version" }):wait()
  assert(exit_code == 0, "superhtml version failed with exit code: " .. tostring(exit_code))
end
test_version()
