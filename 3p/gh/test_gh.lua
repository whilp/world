#!/usr/bin/env run-test.lua
-- teal ignore: test file

local spawn = require("cosmic.spawn")
local path = require("cosmo.path")

local bin = path.join(TEST_DIR, "bin", "gh")

local function test_version()
  local ok, got = spawn({ bin, "--version" }):read()
  assert(ok, "gh --version failed")
  local want = "gh version"
  assert(got:find(want, 1, true), "checking gh, want: " .. want .. " got: " .. got)
end
test_version()
