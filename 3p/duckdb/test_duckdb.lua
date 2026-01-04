#!/usr/bin/env run-test.lua
-- teal ignore: test file

local spawn = require("cosmic.spawn")
local path = require("cosmo.path")

local bin = path.join(TEST_DIR, "duckdb")

local function test_version()
  local ok, got = spawn({ bin, "--version" }):read()
  assert(ok, "duckdb --version failed")
  local want = "v1."
  assert(got:find(want, 1, true), "checking duckdb, want: " .. want .. " got: " .. got)
end
test_version()
