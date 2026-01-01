#!/usr/bin/env lua
local test_file, output = ...
arg = {}

local unix = require("cosmo.unix")
local path = require("cosmo.path")

if not test_file or not output then
  io.stderr:write("usage: test-runner.lua <test_file> <output>\n")
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
unix.unveil("/usr", "rx")
unix.unveil("/proc", "r")
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
unix.unveil(nil, nil)

local lu = require("luaunit")
local cosmo = require("cosmo")

print("# " .. test_file)
dofile(test_file)

local runner = lu.LuaUnit.new()
runner:setOutputType("tap")
local code = runner:runSuite()

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
