#!/usr/bin/env run-test.lua
-- teal ignore: test file

local spawn = require("cosmic.spawn")
local path = require("cosmo.path")

local bin = path.join(TEST_DIR, "sg")

local function test_version()
  local ok, got = spawn({ bin, "--version" }):read()
  assert(ok, "sg --version failed")
  local want = "ast-grep"
  assert(got:find(want, 1, true), "checking ast-grep, want: " .. want .. " got: " .. got)
end
test_version()
