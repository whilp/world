#!/usr/bin/env run-test.lua

local spawn = require("cosmic.spawn")
local path = require("cosmo.path")

global TEST_DIR: string

local bin = path.join(TEST_DIR, "clasp")

local function test_binary_exists()
  local cosmo = require("cosmo")
  local content = cosmo.Slurp(bin)
  assert(content, "clasp binary not found at " .. bin)
  assert(#content > 1000000, "clasp binary seems too small")
end
test_binary_exists()

local function test_version()
  local ok, got = spawn({ bin, "--version" }):read()
  assert(ok, "clasp --version failed: " .. tostring(got))
  assert((got as string):find("%d+%.%d+", 1), "expected version number")
end
test_version()
