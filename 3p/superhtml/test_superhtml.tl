#!/usr/bin/env run-test.lua

global TEST_DIR: string

local spawn = require("cosmic.spawn")
local path = require("cosmo.path")

local bin: string = path.join(TEST_DIR, "superhtml")

local function test_version()
  local exit_code: number = spawn({ bin, "version" }):wait()
  assert(exit_code == 0, "superhtml version failed with exit code: " .. tostring(exit_code))
end
test_version()
