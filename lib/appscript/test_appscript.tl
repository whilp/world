#!/usr/bin/env run-test.lua

local spawn = require("cosmic.spawn")
local path = require("cosmo.path")

global TEST_BIN: string

local bun = path.join(TEST_BIN, "bun")
local srcdir = "lib/appscript"

local function test_js_tests()
  -- run javascript unit tests
  local testfile = path.join(srcdir, "run-tests.js")
  local ok, out = spawn({ bun, "run", testfile }):read()
  if not ok then
    print(out)
  end
  assert(ok, "appscript tests failed")
end

test_js_tests()
