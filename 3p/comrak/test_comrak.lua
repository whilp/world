#!/usr/bin/env run-test.lua

local spawn = require("cosmic.spawn")
local path = require("cosmo.path")

local bin = path.join(TEST_DIR, "comrak")

local function test_version()
  local ok, got = spawn({ bin, "--version" }):read()
  if not ok then skip("binary not executable on this platform") end
  local want = "comrak"
  assert(got:find(want, 1, true), "checking comrak, want: " .. want .. " got: " .. got)
end
test_version()
