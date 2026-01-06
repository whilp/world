#!/usr/bin/env run-test.lua
-- teal ignore: test file

local spawn = require("cosmic.spawn")
local path = require("cosmo.path")

local bin = path.join(TEST_DIR, "bin", "shfmt")

local function test_version()
  local ok, got = spawn({ bin, "--version" }):read()
  assert(ok, "shfmt --version failed")
  local want = "v3"
  assert(got:find(want, 1, true), "checking shfmt, want: " .. want .. " got: " .. got)
end
test_version()
