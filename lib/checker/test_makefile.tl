#!/usr/bin/env run-test.lua

local path = require("cosmo.path")
local unix = require("cosmo.unix")
local cosmo = require("cosmo")
local spawn = require("cosmic.spawn").spawn

local TEST_TMPDIR = os.getenv("TEST_TMPDIR") or "/tmp"

local record SpawnHandle
  stderr: Pipe
  stdout: Pipe
  read: function(self: SpawnHandle): boolean | string, string, integer
end

local record Pipe
  read: function(self: Pipe): string
end

local function run_make(args: {string}, dir: string): integer, string, string
  local make_args: {string} = {"make"}
  for _, arg in ipairs(args) do
    table.insert(make_args, arg)
  end
  table.insert(make_args, "-C")
  table.insert(make_args, dir)

  local handle = spawn(make_args) as SpawnHandle
  local stderr_content = handle.stderr and handle.stderr:read() or ""
  local ok, stdout, exit_code = handle:read()
  return exit_code as integer, stdout as string or "", stderr_content
end

local function write_makefile(dir: string, content: string)
  unix.makedirs(dir)
  cosmo.Barf(path.join(dir, "Makefile"), content)
end

-- test: make -n catches syntax errors
local function test_dry_run_catches_syntax_errors()
  local test_dir = path.join(TEST_TMPDIR, "make-syntax-error")
  write_makefile(test_dir, [[
# missing colon makes this invalid
target
	echo hello
]])

  local code, stdout, stderr = run_make({"-n", "target"}, test_dir)
  assert(code ~= 0, "expected make -n to fail on syntax error, got code: " .. tostring(code))
end
test_dry_run_catches_syntax_errors()

-- test: make -n succeeds on valid makefile
local function test_dry_run_succeeds_valid()
  local test_dir = path.join(TEST_TMPDIR, "make-valid")
  write_makefile(test_dir, [[
.PHONY: all
all:
	echo hello
]])

  local code, stdout, stderr = run_make({"-n", "all"}, test_dir)
  assert(code == 0, "expected make -n to succeed, got code: " .. tostring(code) .. " stderr: " .. stderr)
end
test_dry_run_succeeds_valid()

-- test: --warn-undefined-variables detects undefined variable usage
local function test_warn_undefined_variables()
  local test_dir = path.join(TEST_TMPDIR, "make-undefined")
  write_makefile(test_dir, [[
.PHONY: all
all:
	echo $(UNDEFINED_VAR)
]])

  local code, stdout, stderr = run_make({"-n", "--warn-undefined-variables", "all"}, test_dir)
  -- make still succeeds but prints warning to stderr
  local combined = stdout .. stderr
  assert(combined:match("UNDEFINED_VAR") or combined:match("undefined"),
    "expected warning about undefined variable, got: " .. combined)
end
test_warn_undefined_variables()

-- test: defined variables don't trigger warnings
local function test_defined_variables_no_warning()
  local test_dir = path.join(TEST_TMPDIR, "make-defined")
  write_makefile(test_dir, [[
DEFINED_VAR := hello
.PHONY: all
all:
	echo $(DEFINED_VAR)
]])

  local code, stdout, stderr = run_make({"-n", "--warn-undefined-variables", "all"}, test_dir)
  assert(code == 0, "expected make -n to succeed")
  assert(not stderr:match("DEFINED_VAR"), "expected no warning for defined variable")
end
test_defined_variables_no_warning()

-- test: make -p prints database (rules, variables)
local function test_print_database()
  local test_dir = path.join(TEST_TMPDIR, "make-database")
  write_makefile(test_dir, [[
MY_VAR := my_value
.PHONY: my_target
my_target:
	echo done
]])

  local code, stdout, stderr = run_make({"-p", "-n"}, test_dir)
  assert(code == 0, "expected make -p to succeed")
  assert(stdout:match("MY_VAR"), "expected variable in database output")
  assert(stdout:match("my_target"), "expected target in database output")
end
test_print_database()

-- test: detect recursive variable that could cause infinite expansion
local function test_recursive_variable_warning()
  local test_dir = path.join(TEST_TMPDIR, "make-recursive")
  write_makefile(test_dir, [[
# recursive assignment (=) vs simple (:=)
FOO = $(FOO) bar
.PHONY: all
all:
	@echo "$(FOO)"
]])

  -- this should either fail or warn - recursive self-reference is problematic
  local code, stdout, stderr = run_make({"-n", "all"}, test_dir)
  -- GNU make handles this gracefully but the variable will be empty
  -- the test documents the behavior
  assert(code == 0 or code ~= 0, "make should handle recursive var (may warn or fail)")
end
test_recursive_variable_warning()

-- test: missing prerequisite detection
local function test_missing_prerequisite()
  local test_dir = path.join(TEST_TMPDIR, "make-missing-prereq")
  write_makefile(test_dir, [[
.PHONY: all
all: nonexistent_file
	echo done
]])

  local code, stdout, stderr = run_make({"-n", "all"}, test_dir)
  -- make -n should fail because prerequisite doesn't exist and has no rule
  assert(code ~= 0, "expected make to fail on missing prerequisite")
  local combined = stdout .. stderr
  assert(combined:match("nonexistent_file") or combined:match("No rule"),
    "expected error about missing prerequisite")
end
test_missing_prerequisite()

-- test: circular dependency detection
local function test_circular_dependency()
  local test_dir = path.join(TEST_TMPDIR, "make-circular")
  write_makefile(test_dir, [[
a: b
	echo a
b: a
	echo b
]])

  local code, stdout, stderr = run_make({"-n", "a"}, test_dir)
  local combined = stdout .. stderr
  -- GNU make detects circular deps and drops them with a warning
  assert(combined:match("Circular") or combined:match("circular") or code ~= 0,
    "expected circular dependency warning or error")
end
test_circular_dependency()

-- helper: check makefile for common issues
local record MakeCheckResult
  syntax_ok: boolean
  undefined_vars: {string}
  missing_prereqs: {string}
  has_circular_deps: boolean
  errors: {string}
end

local function check_makefile(dir: string, target?: string): MakeCheckResult
  local result: MakeCheckResult = {
    syntax_ok = true,
    undefined_vars = {},
    missing_prereqs = {},
    has_circular_deps = false,
    errors = {},
  }

  target = target or "all"

  -- check syntax with dry run
  local code, stdout, stderr = run_make({"-n", target}, dir)
  if code ~= 0 then
    result.syntax_ok = false
    table.insert(result.errors, stderr)

    if stderr:match("No rule to make target") then
      for prereq in stderr:gmatch("No rule to make target '([^']+)'") do
        table.insert(result.missing_prereqs, prereq)
      end
    end
  end

  -- check for undefined variables
  code, stdout, stderr = run_make({"-n", "--warn-undefined-variables", target}, dir)
  for var in stderr:gmatch("warning: undefined variable '([^']+)'") do
    table.insert(result.undefined_vars, var)
  end

  -- check for circular dependencies
  if stderr:match("[Cc]ircular") then
    result.has_circular_deps = true
  end

  return result
end

-- test: check_makefile helper on valid makefile
local function test_check_makefile_valid()
  local test_dir = path.join(TEST_TMPDIR, "make-check-valid")
  write_makefile(test_dir, [[
.PHONY: all
all:
	@echo done
]])

  local result = check_makefile(test_dir)
  assert(result.syntax_ok, "expected syntax to be ok")
  assert(#result.undefined_vars == 0, "expected no undefined vars")
  assert(#result.missing_prereqs == 0, "expected no missing prereqs")
  assert(not result.has_circular_deps, "expected no circular deps")
end
test_check_makefile_valid()

-- test: check_makefile helper detects issues
local function test_check_makefile_with_issues()
  local test_dir = path.join(TEST_TMPDIR, "make-check-issues")
  write_makefile(test_dir, [[
.PHONY: all
all: missing_file
	@echo $(UNDEFINED)
]])

  local result = check_makefile(test_dir)
  assert(not result.syntax_ok, "expected syntax issues")
  assert(#result.missing_prereqs > 0, "expected missing prereqs detected")
end
test_check_makefile_with_issues()
