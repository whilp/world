#!/usr/bin/env lua
local unix = require("cosmo.unix")
local path = require("cosmo.path")
local cosmo = require("cosmo")
local walk = require("walk")

local function run_tests(test_file, output, extra_args)
  _G.arg = {}
  TEST_ARGS = extra_args or {}
  TEST_TMPDIR = nil

  local start_sec, start_nsec = unix.clock_gettime(0)

  local output_dir = path.dirname(output)
  unix.makedirs(output_dir)
  TEST_TMPDIR = unix.mkdtemp("/tmp/test_XXXXXX")
  unix.unveil(test_file, "r")
  unix.unveil(output_dir, "rwc")
  unix.unveil("o", "rx")
  unix.unveil("lib", "r")
  unix.unveil("3p", "r")
  unix.unveil(TEST_TMPDIR, "rwc")
  unix.unveil("/dev/null", "rw")
  unix.unveil("/proc", "r")
  unix.unveil("/usr", "rx")
  unix.unveil("/etc", "r")
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
  for _, p in ipairs(TEST_ARGS) do
    unix.unveil(p, "r")
  end
  unix.unveil(nil, nil)

  local lu = require("luaunit")

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
    unix.rmrf(TEST_TMPDIR)
    os.exit(1)
  end

  local runner = lu.LuaUnit.new()
  runner:setOutputType("tap")
  local code = runner:runSuite()

  unix.rmrf(TEST_TMPDIR)

  if runner.result.runCount == 0 and runner.result.skippedCount == 0 then
    io.stderr:write("error: no tests found in " .. test_file .. "\n")
    os.exit(1)
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

  os.exit(code)
end

local function report(output_dir)
  local files = {}
  local total_tests = 0
  local total_passed = 0
  local total_failed = 0
  local total_errors = 0
  local total_skipped = 0
  local total_duration_ms = 0

  for _, filepath in ipairs(walk.collect(output_dir, "%.luatest%.ok$")) do
    local chunk = loadfile(filepath)
    if chunk then
      local result = chunk()
      if result then
        table.insert(files, result)
        total_tests = total_tests + result.tests
        total_passed = total_passed + result.passed
        total_failed = total_failed + result.failed
        total_errors = total_errors + result.errors
        total_skipped = total_skipped + result.skipped
        total_duration_ms = total_duration_ms + (result.duration_ms or 0)
      end
    end
  end

  local file_count = #files

  print("luatest report")
  print("───────────────────────────────")
  print(string.format("  test files:     %d", file_count))
  print(string.format("  total tests:    %d", total_tests))
  print(string.format("  passed:         %d", total_passed))
  print(string.format("  failed:         %d", total_failed))
  print(string.format("  errors:         %d", total_errors))
  print(string.format("  skipped:        %d", total_skipped))
  print(string.format("  duration:       %.2fms", total_duration_ms))
  print("")

  if total_failed > 0 or total_errors > 0 then
    print("files with failures:")
    table.sort(files, function(a, b) return a.file < b.file end)
    for _, f in ipairs(files) do
      if f.failed > 0 or f.errors > 0 then
        print(string.format("  %s (failed: %d, errors: %d, %.2fms)", f.file, f.failed, f.errors, f.duration_ms or 0))
      end
    end
  end

  if file_count > 1 then
    print("slowest test files:")
    table.sort(files, function(a, b)
      return (a.duration_ms or 0) > (b.duration_ms or 0)
    end)
    local show_count = math.min(5, file_count)
    for i = 1, show_count do
      local f = files[i]
      print(string.format("  %s (%.2fms, %d tests)", f.file, f.duration_ms or 0, f.tests))
    end
  end

  return (total_failed == 0 and total_errors == 0)
end

local function main(args)
  local cmd = args[1]

  if cmd == "report" then
    local output_dir = args[2] or "o/any"
    return report(output_dir) and 0 or 1
  end

  local test_file, output = args[1], args[2]
  if not test_file or not output then
    io.stderr:write("usage: luatest.lua <test_file> <output> [unveil_paths...]\n")
    io.stderr:write("       luatest.lua report [output_dir]\n")
    return 1
  end

  local extra_args = {}
  for i = 3, #args do
    extra_args[#extra_args + 1] = args[i]
  end

  run_tests(test_file, output, extra_args)
  return 0
end

if ... then
  os.exit(main({ ... }))
else
  return {
    run_tests = run_tests,
    report = report,
    main = main,
  }
end
