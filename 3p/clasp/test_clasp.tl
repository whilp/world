#!/usr/bin/env run-test.lua

local spawn = require("cosmic.child").spawn
local path = require("cosmo.path")

global TEST_BIN: string

local bin = path.join(TEST_BIN, "clasp")

local function test_version()
  -- version may be "unknown" when compiled with bun (package.json not embedded)
  local ok, got = spawn({ bin, "--version" }):read()
  assert(ok, "clasp --version failed")
  assert((got as string):match("%S"), "clasp --version returned empty output")
end

local function test_help()
  local ok, got = spawn({ bin, "--help" }):read()
  assert(ok, "clasp --help failed")
  local want = "Apps Script"
  assert((got as string):find(want, 1, true), "checking clasp help, want: " .. want .. " got: " .. (got as string))
end

test_version()
test_help()
