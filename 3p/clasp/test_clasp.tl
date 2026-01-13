#!/usr/bin/env run-test.lua

local spawn = require("cosmic.spawn")
local path = require("cosmo.path")

global TEST_BIN: string

local bin = path.join(TEST_BIN, "clasp")

local function test_version()
  local ok, got = spawn({ bin, "--version" }):read()
  assert(ok, "clasp --version failed")
  local want = "3.1"
  assert((got as string):find(want, 1, true), "checking clasp version, want: " .. want .. " got: " .. (got as string))
end

local function test_help()
  local ok, got = spawn({ bin, "--help" }):read()
  assert(ok, "clasp --help failed")
  local want = "Apps Script"
  assert((got as string):find(want, 1, true), "checking clasp help, want: " .. want .. " got: " .. (got as string))
end

test_version()
test_help()
