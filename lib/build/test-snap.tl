#!/usr/bin/env cosmic
-- Snapshot testing: compare expected vs actual output

local cosmo = require("cosmo")
local spawn = require("cosmic.spawn")
local unix = require("cosmo.unix")

local function run_diff(expected: string, actual: string): number, string
  local handle = spawn({"diff", "-u", expected, actual})
  local ok, stdout, exit_code = handle:read()
  -- diff returns 0 if files match, 1 if different, 2 on error
  return exit_code, stdout or ""
end

local function main(args: {string}): number, string
  if #args < 2 then
    return 1, "Usage: test-snap.lua <expected.snap> <actual.snap>"
  end

  local expected = args[1]
  local actual = args[2]

  -- Check both files exist
  if not unix.stat(expected) then
    return 1, string.format("Expected file not found: %s", expected)
  end
  if not unix.stat(actual) then
    return 1, string.format("Actual file not found: %s", actual)
  end

  local exit_code, stdout = run_diff(expected, actual)

  if exit_code == 0 then
    -- Files match
    print("pass")
    print("")
    print("## stdout")
    print("")
    print("## stderr")
    return 0
  elseif exit_code == 1 then
    -- Files differ
    print("fail: snapshot mismatch")
    print("")
    print("## stdout")
    print("")
    print("## stderr")
    print(stdout)
    return 1
  else
    -- diff command failed
    return 1, string.format("diff failed with exit code %d", exit_code)
  end
end

if cosmo.is_main() then
  local exit_code, err = main(arg)
  if err then
    io.stderr:write(err .. "\n")
  end
  os.exit(exit_code)
end
