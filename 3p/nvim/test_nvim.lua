#!/usr/bin/env run-test.lua

local spawn = require("cosmic.spawn")
local path = require("cosmo.path")

local bin = path.join(TEST_DIR, "bin", "nvim")

local function test_version()
  local ok, got = spawn({ bin, "--version" }):read()
  assert(ok, "nvim --version failed")
  local want = "NVIM"
  assert(got:find(want, 1, true), "checking nvim, want: " .. want .. " got: " .. got)
end
test_version()
