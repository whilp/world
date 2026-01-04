#!/usr/bin/env run-test.lua
-- teal ignore: test file

local spawn = require("cosmic.spawn")
local path = require("cosmo.path")

local bin = path.join(TEST_DIR, "stylua")

local function test_version()
  local ok, got = spawn({ bin, "--version" }):read()
  assert(ok, "stylua --version failed")
  local want = "stylua"
  assert(got:find(want, 1, true), "checking stylua, want: " .. want .. " got: " .. got)
end
test_version()
