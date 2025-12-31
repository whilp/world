#!/usr/bin/env lua
-- save args before clearing
local test_file, bin_dir, output = ...

-- clear arg so luaunit doesn't try to parse command line
arg = {}

local lu = require("luaunit")
local cosmo = require("cosmo")

if not test_file or not bin_dir or not output then
  io.stderr:write("usage: test-runner.lua <test_file> <bin_dir> <output>\n")
  os.exit(1)
end

-- load test suite, passing bin_dir
local suite = dofile(test_file)
if type(suite) == "function" then
  suite = suite(bin_dir)
end

-- run tests
local runner = lu.LuaUnit.new()
runner:setOutputType("nil")  -- quiet
local code = runner:runSuite()

-- write results
local result = {
  tests = runner.result.runCount,
  passed = runner.result.runCount - runner.result.notSuccessCount - runner.result.skippedCount,
  failed = runner.result.failureCount,
  errors = runner.result.errorCount,
  skipped = runner.result.skippedCount,
}

local f = io.open(output, "w")
f:write("return " .. cosmo.EncodeLua(result) .. "\n")
f:close()

os.exit(code)
