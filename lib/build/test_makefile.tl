#!/usr/bin/env run-test.lua
-- Test the actual repo Makefile using GNU make's validation features

local spawn = require("cosmic.spawn").spawn

local record SpawnHandle
  stderr: {read: function(self: any): string}
  read: function(self: SpawnHandle): boolean, string, integer
  wait: function(self: SpawnHandle): integer
end

local function run_make(...: string): integer, string
  local args: {string} = {"make"}
  local varargs: {string} = {...}
  for _, arg in ipairs(varargs) do
    table.insert(args, arg)
  end

  local handle = spawn(args) as SpawnHandle
  local ok, stdout, exit_code = handle:read()
  return exit_code, stdout or ""
end

-- test: make -n succeeds on repo Makefile (dry run)
local function test_dry_run_succeeds()
  local exit_code, stdout = run_make("-n", "files")
  assert(exit_code == 0, "expected make -n files to succeed, got code: " .. tostring(exit_code))
end
test_dry_run_succeeds()

-- test: make -n help works
local function test_help_dry_run()
  local exit_code = run_make("-n", "help")
  assert(exit_code == 0, "expected make -n help to succeed, got code: " .. tostring(exit_code))
end
test_help_dry_run()

-- test: make -n test works
local function test_test_dry_run()
  local exit_code = run_make("-n", "test")
  assert(exit_code == 0, "expected make -n test to succeed, got code: " .. tostring(exit_code))
end
test_test_dry_run()

-- test: make -n check works
local function test_check_dry_run()
  local exit_code = run_make("-n", "check")
  assert(exit_code == 0, "expected make -n check to succeed, got code: " .. tostring(exit_code))
end
test_check_dry_run()

-- test: make -p prints database (rules, variables)
local function test_print_database()
  local exit_code, stdout = run_make("-p", "-n", "-q")
  -- -q may return non-zero if targets are out of date, but output should have content
  assert(stdout:match("modules"), "expected 'modules' variable in database output")
  assert(stdout:match("%.PHONY"), "expected .PHONY in database output")
end
test_print_database()

-- test: all major targets can be dry-run
local function test_all_major_targets()
  local targets = {"files", "staged", "fetched", "clean", "bootstrap", "build"}
  for _, target in ipairs(targets) do
    local exit_code = run_make("-n", target)
    assert(exit_code == 0, "expected make -n " .. target .. " to succeed, got code: " .. tostring(exit_code))
  end
end
test_all_major_targets()
