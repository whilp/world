#!/usr/bin/env run-test.lua
local spawn = require("cosmic.spawn")
local path = require("cosmo.path")

global TEST_DIR: string

local cosmic = path.join(os.getenv("TEST_BIN"), "cosmic")
local tl_script = path.join(TEST_DIR, "tl")

local function test_version()
  -- use "--" to separate cosmic args from tl args
  local handle = spawn({ cosmic, "--", tl_script, "--version" })
  assert(handle ~= nil, "spawn should succeed")
  local exit_code = handle:wait()
  assert(exit_code == 0, "expected exit code 0, got " .. tostring(exit_code))
end
test_version()
