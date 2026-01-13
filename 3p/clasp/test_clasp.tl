#!/usr/bin/env run-test.lua

local spawn = require("cosmic.spawn")
local path = require("cosmo.path")
local unix = require("cosmo.unix")

local test_bin = os.getenv("TEST_BIN") or ""
local bin = path.join(test_bin, "clasp")

local function test_script_exists()
  local cosmo = require("cosmo")
  local content = cosmo.Slurp(bin)
  assert(content, "clasp script not found at " .. bin)
  assert(content:find("bun", 1, true), "clasp script should contain bun")
end
test_script_exists()

-- version test requires npm access - skip in CI
local function test_version()
  local ok, got = spawn({ bin, "--version" }):read()
  local output = got or ""
  -- skip if deps not installed (npm blocked)
  if not ok or output == "" or output:find("403") then
    assert(false, "SKIP npm registry blocked")
    return
  end
  assert(output:find("%d+%.%d+", 1), "expected version number")
end
test_version()
