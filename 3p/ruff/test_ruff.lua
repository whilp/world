#!/usr/bin/env run-test.lua
-- teal ignore: test file

local spawn = require("cosmic.spawn")
local path = require("cosmo.path")

local bin = path.join(TEST_DIR, "ruff")

local function test_version()
  local ok, got = spawn({ bin, "--version" }):read()
  assert(ok, "ruff --version failed")
  local want = "ruff"
  assert(got:find(want, 1, true), "checking ruff, want: " .. want .. " got: " .. got)
end
test_version()
