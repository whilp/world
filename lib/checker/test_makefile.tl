#!/usr/bin/env run-test.lua

local path = require("cosmo.path")
local cosmo = require("cosmo")

global TEST_DIR: string

local record MakeOutput
  output: string
  exit_code: number
end

local function read_make_output(filename: string): MakeOutput
  local filepath = path.join(TEST_DIR, filename)
  local content = cosmo.Slurp(filepath)
  assert(content, "failed to read " .. filepath)

  local exit_code = content:match("exit:(%d+)%s*$")
  assert(exit_code, "missing exit code in " .. filepath)

  local output = content:gsub("exit:%d+%s*$", "")
  return {
    output = output,
    exit_code = tonumber(exit_code),
  }
end

-- test: make -n catches syntax errors
local function test_dry_run_catches_syntax_errors()
  local result = read_make_output("syntax-error.dry-run.out")
  assert(result.exit_code ~= 0, "expected make -n to fail on syntax error, got code: " .. tostring(result.exit_code))
end
test_dry_run_catches_syntax_errors()

-- test: make -n succeeds on valid makefile
local function test_dry_run_succeeds_valid()
  local result = read_make_output("valid.dry-run.out")
  assert(result.exit_code == 0, "expected make -n to succeed, got code: " .. tostring(result.exit_code) .. " output: " .. result.output)
end
test_dry_run_succeeds_valid()

-- test: --warn-undefined-variables detects undefined variable usage
local function test_warn_undefined_variables()
  local result = read_make_output("undefined-var.warn-undef.out")
  assert(result.output:match("UNDEFINED_VAR") or result.output:match("undefined"),
    "expected warning about undefined variable, got: " .. result.output)
end
test_warn_undefined_variables()

-- test: defined variables don't trigger warnings
local function test_defined_variables_no_warning()
  local result = read_make_output("defined-var.warn-undef.out")
  assert(result.exit_code == 0, "expected make -n to succeed")
  assert(not result.output:match("warning.*DEFINED_VAR"), "expected no warning for defined variable")
end
test_defined_variables_no_warning()

-- test: make -p prints database (rules, variables)
local function test_print_database()
  local result = read_make_output("database.database.out")
  assert(result.exit_code == 0, "expected make -p to succeed")
  assert(result.output:match("MY_VAR"), "expected variable in database output")
  assert(result.output:match("my_target"), "expected target in database output")
end
test_print_database()

-- test: missing prerequisite detection
local function test_missing_prerequisite()
  local result = read_make_output("missing-prereq.dry-run.out")
  assert(result.exit_code ~= 0, "expected make to fail on missing prerequisite")
  assert(result.output:match("nonexistent_file") or result.output:match("No rule"),
    "expected error about missing prerequisite, got: " .. result.output)
end
test_missing_prerequisite()

-- test: circular dependency detection
local function test_circular_dependency()
  local result = read_make_output("circular-dep.dry-run.out")
  assert(result.output:match("[Cc]ircular") or result.exit_code ~= 0,
    "expected circular dependency warning or error, got: " .. result.output)
end
test_circular_dependency()

-- helper: parse make output for common issues
local record MakeCheckResult
  syntax_ok: boolean
  undefined_vars: {string}
  missing_prereqs: {string}
  has_circular_deps: boolean
end

local function parse_make_results(dry_run: MakeOutput, warn_undef: MakeOutput): MakeCheckResult
  local result: MakeCheckResult = {
    syntax_ok = dry_run.exit_code == 0,
    undefined_vars = {},
    missing_prereqs = {},
    has_circular_deps = false,
  }

  -- extract missing prerequisites
  if dry_run.output:match("No rule to make target") then
    for prereq in dry_run.output:gmatch("No rule to make target '([^']+)'") do
      table.insert(result.missing_prereqs, prereq)
    end
  end

  -- extract undefined variables
  for var in warn_undef.output:gmatch("warning: undefined variable '([^']+)'") do
    table.insert(result.undefined_vars, var)
  end

  -- check for circular dependencies
  if dry_run.output:match("[Cc]ircular") or warn_undef.output:match("[Cc]ircular") then
    result.has_circular_deps = true
  end

  return result
end

-- test: parse_make_results helper on valid makefile
local function test_parse_results_valid()
  local dry_run = read_make_output("valid.dry-run.out")
  local warn_undef = read_make_output("valid.warn-undef.out")
  local result = parse_make_results(dry_run, warn_undef)

  assert(result.syntax_ok, "expected syntax to be ok")
  assert(#result.undefined_vars == 0, "expected no undefined vars")
  assert(#result.missing_prereqs == 0, "expected no missing prereqs")
  assert(not result.has_circular_deps, "expected no circular deps")
end
test_parse_results_valid()

-- test: parse_make_results helper detects issues
local function test_parse_results_with_issues()
  local dry_run = read_make_output("missing-prereq.dry-run.out")
  local warn_undef = read_make_output("missing-prereq.warn-undef.out")
  local result = parse_make_results(dry_run, warn_undef)

  assert(not result.syntax_ok, "expected syntax issues")
  assert(#result.missing_prereqs > 0, "expected missing prereqs detected")
end
test_parse_results_with_issues()
