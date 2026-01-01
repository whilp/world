#!/usr/bin/env lua
local args = { ... }
local test_file, output = args[1], args[2]
arg = {}
TEST_ARGS = {}
TEST_TMPDIR = nil -- set after mkdtemp
for i = 3, #args do
  TEST_ARGS[#TEST_ARGS + 1] = args[i]
end

local unix = require("cosmo.unix")
local path = require("cosmo.path")

if not test_file or not output then
  io.stderr:write("usage: test-runner.lua <test_file> <output> [unveil_paths...]\n")
  os.exit(1)
end

local output_dir = path.dirname(output)
unix.makedirs(output_dir)
unix.unveil(test_file, "r")
unix.unveil(output_dir, "rwc")
unix.unveil("o", "rx")
unix.unveil("lib", "r")
unix.unveil("3p", "r")
unix.unveil("/tmp", "rwc")
unix.unveil("/proc", "r")
unix.unveil("/usr", "rx")
unix.unveil("/etc", "r")
-- APE binaries need the APE loader from $HOME/.ape-*
local home = os.getenv("HOME")
if home then
  local dir = unix.opendir(home)
  if dir then
    for name in dir do
      if name:match("^%.ape%-") then
        unix.unveil(path.join(home, name), "rx")
      end
    end
  end
end
-- setup TEST_TMPDIR for tests to use
TEST_TMPDIR = unix.mkdtemp("/tmp/test_XXXXXX")
unix.unveil(TEST_TMPDIR, "rwc")
-- Unveil additional paths passed as arguments (available to test via TEST_ARGS)
for _, p in ipairs(TEST_ARGS) do
  unix.unveil(p, "r")
end
unix.unveil(nil, nil)

local lu = require("luaunit")
local cosmo = require("cosmo")

-- exclude TEST_* globals from test discovery (reserved for test runner)
local original_isTestName = lu.LuaUnit.isTestName
lu.LuaUnit.isTestName = function(s)
  if s:match("^TEST_") then
    return false
  end
  return original_isTestName(s)
end

print("# " .. test_file)
local ok, err = pcall(dofile, test_file)
if not ok then
  io.stderr:write("error loading test file: " .. tostring(err) .. "\n")
  os.exit(1)
end

local runner = lu.LuaUnit.new()
runner:setOutputType("tap")
local code = runner:runSuite()

-- cleanup TEST_TMPDIR
unix.rmrf(TEST_TMPDIR)

if runner.result.runCount == 0 and runner.result.skippedCount == 0 then
  io.stderr:write("error: no tests found in " .. test_file .. "\n")
  os.exit(1)
end

local result = {
  file = test_file,
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
