#!/usr/bin/env lua
local test_file, output = ...
arg = {}

local lu = require("luaunit")
local cosmo = require("cosmo")

if not test_file or not output then
  io.stderr:write("usage: test-runner.lua <test_file> <output>\n")
  os.exit(1)
end

print("# " .. test_file)
dofile(test_file)

local runner = lu.LuaUnit.new()
runner:setOutputType("tap")
local code = runner:runSuite()

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
