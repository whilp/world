#!/usr/bin/env run-test.lua
-- teal ignore: test file

local spawn = require("cosmic.spawn")
local path = require("cosmo.path")

local bin = path.join(TEST_DIR, "tree-sitter")

local function test_version()
  local ok, got = spawn({ bin, "--version" }):read()
  assert(ok, "tree-sitter --version failed")
  local want = "tree-sitter"
  assert(got:find(want, 1, true), "checking tree-sitter, want: " .. want .. " got: " .. got)
end
test_version()
