#!/usr/bin/env run-test.lua

global TEST_DIR: string

local spawn = require("cosmic.spawn")
local path = require("cosmo.path")

local bin: string = path.join(TEST_DIR, "bin", "tree-sitter")

local function test_version()
  local ok, got = spawn({ bin, "--version" }):read()
  assert(ok, "tree-sitter --version failed")
  local want = "tree-sitter"
  assert((got as string):find(want, 1, true), "checking tree-sitter, want: " .. want .. " got: " .. (got as string))
end
test_version()
