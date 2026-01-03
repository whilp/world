#!/usr/bin/env lua

local unix = require("cosmo.unix")
local path = require("cosmo.path")
local cosmo = require("cosmo")

local function main(test_file, output)
  _G.arg = {}

  if not test_file or not output then
    return 1, "usage: run-luatest <test_file> <output.ok>"
  end

  local start_sec, start_nsec = unix.clock_gettime(0)

  local output_dir = path.dirname(output)
  unix.makedirs(output_dir)

  TEST_TMPDIR = unix.mkdtemp("/tmp/test_XXXXXX")

  local lu = require("luaunit")

  -- skip TEST_* constants
  local original_isTestName = lu.LuaUnit.isTestName
  lu.LuaUnit.isTestName = function(s)
    if s:match("^TEST_") then
      return false
    end
    return original_isTestName(s)
  end

  local ok, err = pcall(dofile, test_file)
  if not ok then
    unix.rmrf(TEST_TMPDIR)
    return 1, "error loading test file: " .. tostring(err)
  end

  local runner = lu.LuaUnit.new()
  runner:setOutputType("tap")
  local code = runner:runSuite()

  unix.rmrf(TEST_TMPDIR)

  if runner.result.runCount == 0 and runner.result.skippedCount == 0 then
    return 1, "error: no tests found in " .. test_file
  end

  local end_sec, end_nsec = unix.clock_gettime(0)
  local duration_sec = end_sec - start_sec
  local duration_nsec = end_nsec - start_nsec
  if duration_nsec < 0 then
    duration_sec = duration_sec - 1
    duration_nsec = duration_nsec + 1000000000
  end
  local duration_ms = duration_sec * 1000 + duration_nsec / 1000000

  local result = {
    file = test_file,
    tests = runner.result.runCount,
    passed = runner.result.runCount - runner.result.notSuccessCount - runner.result.skippedCount,
    failed = runner.result.failureCount,
    errors = runner.result.errorCount,
    skipped = runner.result.skippedCount,
    duration_ms = duration_ms,
  }

  cosmo.Barf(output, "return " .. cosmo.EncodeLua(result) .. "\n")

  return code
end

if cosmo.is_main() then
  local code, err = main(...)
  if err then
    io.stderr:write(err .. "\n")
  end
  os.exit(code)
end
