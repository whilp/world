local lu = require("luaunit")
local cosmo = require("cosmo")
local path = require("cosmo.path")
local unix = require("cosmo.unix")
local spawn = require("spawn").spawn

local luacheck_bin = path.join(os.getenv("TEST_BIN_DIR"), "bin", "luacheck")
local luacheck_script = "lib/build/luacheck.lua"
local lua_bin = "o/any/lua/bin/lua"

local function run_check(code, filename)
  local filepath = path.join(TEST_TMPDIR, filename)
  local output_path = path.join(TEST_TMPDIR, filename .. ".luacheck.ok")
  cosmo.Barf(filepath, code)

  local handle = spawn({ lua_bin, luacheck_script, filepath, output_path, luacheck_bin })
  handle:wait()

  local chunk = loadfile(output_path)
  if not chunk then
    return nil, "failed to load output"
  end
  return chunk()
end

TestLuacheckCheck = {}

function TestLuacheckCheck:test_clean_code_passes()
  local result = run_check([[
local x = 10
return x
]], "clean.lua")
  lu.assertNotNil(result)
  lu.assertTrue(result.passed)
  lu.assertEquals(#result.issues, 0)
end

function TestLuacheckCheck:test_undefined_global_detected()
  local result = run_check([[
local x = 10
y = 20
return x + y
]], "global_bad.lua")
  lu.assertNotNil(result)
  lu.assertFalse(result.passed)
  lu.assertTrue(#result.issues > 0)
  lu.assertEquals(result.issues[1].code, "W111")
end

function TestLuacheckCheck:test_unused_variable_detected()
  local result = run_check([[
local x = 10
local y = 20
return y
]], "unused_bad.lua")
  lu.assertNotNil(result)
  lu.assertFalse(result.passed)
  local codes = {}
  for _, issue in ipairs(result.issues) do codes[issue.code] = true end
  lu.assertTrue(codes["W211"], "should detect unused variable W211")
end

function TestLuacheckCheck:test_variable_shadowing_detected()
  local result = run_check([[
local x = 10
local function foo()
  local x = 20
  return x
end
return foo() + x
]], "shadow_bad.lua")
  lu.assertNotNil(result)
  lu.assertFalse(result.passed)
  local codes = {}
  for _, issue in ipairs(result.issues) do codes[issue.code] = true end
  lu.assertTrue(codes["W431"], "should detect shadowing upvalue W431")
end

function TestLuacheckCheck:test_issues_have_location()
  local result = run_check([[
y = 10
]], "location.lua")
  lu.assertNotNil(result)
  lu.assertTrue(#result.issues > 0)
  local issue = result.issues[1]
  lu.assertNotNil(issue.line)
  lu.assertNotNil(issue.column)
  lu.assertNotNil(issue.code)
  lu.assertNotNil(issue.message)
end

function TestLuacheckCheck:test_line_too_long_detected()
  local long_str = string.rep("x", 130)
  local result = run_check('local v = "' .. long_str .. '"\nreturn v\n', "long_line.lua")
  lu.assertNotNil(result)
  lu.assertFalse(result.passed)
end

local luacheck_module = dofile(luacheck_script)

TestParsePlain = {}

function TestParsePlain:test_parse_with_ranges()
  local output = "test.lua:10:5-10: (W111) setting non-standard global variable foo"
  local issues = luacheck_module.parse_plain(output)

  lu.assertEquals(#issues, 1)
  lu.assertEquals(issues[1].line, 10)
  lu.assertEquals(issues[1].column, 5)
  lu.assertEquals(issues[1].end_column, 10)
  lu.assertEquals(issues[1].code, "W111")
  lu.assertEquals(issues[1].message, "setting non-standard global variable foo")
end

function TestParsePlain:test_parse_without_ranges()
  local output = "test.lua:5:3: (W211) unused variable x"
  local issues = luacheck_module.parse_plain(output)

  lu.assertEquals(#issues, 1)
  lu.assertEquals(issues[1].line, 5)
  lu.assertEquals(issues[1].column, 3)
  lu.assertNil(issues[1].end_column)
  lu.assertEquals(issues[1].code, "W211")
  lu.assertEquals(issues[1].message, "unused variable x")
end

function TestParsePlain:test_parse_multiple_issues()
  local output = [[test.lua:10:5: (W111) global foo
test.lua:15:3: (W211) unused x
test.lua:20:8-12: (E111) error here]]
  local issues = luacheck_module.parse_plain(output)

  lu.assertEquals(#issues, 3)
  lu.assertEquals(issues[1].code, "W111")
  lu.assertEquals(issues[2].code, "W211")
  lu.assertEquals(issues[3].code, "E111")
end

function TestParsePlain:test_parse_empty_output()
  local issues = luacheck_module.parse_plain("")
  lu.assertEquals(#issues, 0)
end

function TestParsePlain:test_parse_nil_output()
  local issues = luacheck_module.parse_plain(nil)
  lu.assertEquals(#issues, 0)
end

function TestParsePlain:test_parse_ignores_invalid_lines()
  local output = [[test.lua:10:5: (W111) valid issue
this is not a valid line
test.lua:15:3: (W211) another valid issue]]
  local issues = luacheck_module.parse_plain(output)

  lu.assertEquals(#issues, 2)
end

TestReport = {}

function TestReport:test_report_all_passed()
  local test_dir = path.join(TEST_TMPDIR, "report_pass")
  unix.makedirs(test_dir)

  local result1 = {
    file = "test1.lua",
    checker = "luacheck",
    passed = true,
    exit_code = 0,
    issues = {}
  }
  cosmo.Barf(path.join(test_dir, "test1.luacheck.ok"), "return " .. cosmo.EncodeLua(result1) .. "\n")

  local result2 = {
    file = "test2.lua",
    checker = "luacheck",
    passed = true,
    exit_code = 0,
    issues = {}
  }
  cosmo.Barf(path.join(test_dir, "test2.luacheck.ok"), "return " .. cosmo.EncodeLua(result2) .. "\n")

  local success = luacheck_module.report(test_dir)
  lu.assertTrue(success)
end

function TestReport:test_report_some_failed()
  local test_dir = path.join(TEST_TMPDIR, "report_fail")
  unix.makedirs(test_dir)

  local result1 = {
    file = "test1.lua",
    checker = "luacheck",
    passed = false,
    exit_code = 1,
    issues = {
      { line = 10, column = 5, code = "W111", message = "global foo" }
    }
  }
  cosmo.Barf(path.join(test_dir, "test1.luacheck.ok"), "return " .. cosmo.EncodeLua(result1) .. "\n")

  local success = luacheck_module.report(test_dir)
  lu.assertFalse(success)
end

function TestReport:test_report_counts_issues()
  local test_dir = path.join(TEST_TMPDIR, "report_counts")
  unix.makedirs(test_dir)

  local result1 = {
    file = "test1.lua",
    checker = "luacheck",
    passed = false,
    exit_code = 1,
    issues = {
      { line = 10, column = 5, code = "W111", message = "global foo" },
      { line = 15, column = 3, code = "W211", message = "unused x" }
    }
  }
  cosmo.Barf(path.join(test_dir, "test1.luacheck.ok"), "return " .. cosmo.EncodeLua(result1) .. "\n")

  local result2 = {
    file = "test2.lua",
    checker = "luacheck",
    passed = false,
    exit_code = 1,
    issues = {
      { line = 5, column = 2, code = "W111", message = "global bar" }
    }
  }
  cosmo.Barf(path.join(test_dir, "test2.luacheck.ok"), "return " .. cosmo.EncodeLua(result2) .. "\n")

  local success = luacheck_module.report(test_dir)
  lu.assertFalse(success)
end

function TestReport:test_report_empty_directory()
  local test_dir = path.join(TEST_TMPDIR, "report_empty")
  unix.makedirs(test_dir)

  local success = luacheck_module.report(test_dir)
  lu.assertTrue(success)
end

TestExitCode = {}

function TestExitCode:test_main_returns_0_on_pass()
  local test_file = path.join(TEST_TMPDIR, "pass.lua")
  local output_file = path.join(TEST_TMPDIR, "pass.lua.luacheck.ok")
  cosmo.Barf(test_file, "local x = 10\nreturn x\n")

  local exit_code = luacheck_module.main({ test_file, output_file, luacheck_bin })
  lu.assertEquals(exit_code, 0)
end

function TestExitCode:test_main_returns_1_on_fail()
  local test_file = path.join(TEST_TMPDIR, "fail.lua")
  local output_file = path.join(TEST_TMPDIR, "fail.lua.luacheck.ok")
  cosmo.Barf(test_file, "local unused = 10\nreturn 5\n")

  local exit_code = luacheck_module.main({ test_file, output_file, luacheck_bin })
  lu.assertEquals(exit_code, 1)
end

function TestExitCode:test_main_report_returns_0_when_all_pass()
  local test_dir = path.join(TEST_TMPDIR, "exit_pass")
  unix.makedirs(test_dir)

  local result = {
    file = "test.lua",
    checker = "luacheck",
    passed = true,
    exit_code = 0,
    issues = {}
  }
  cosmo.Barf(path.join(test_dir, "test.luacheck.ok"), "return " .. cosmo.EncodeLua(result) .. "\n")

  local exit_code = luacheck_module.main({ "report", test_dir })
  lu.assertEquals(exit_code, 0)
end

function TestExitCode:test_main_report_returns_1_when_any_fail()
  local test_dir = path.join(TEST_TMPDIR, "exit_fail")
  unix.makedirs(test_dir)

  local result = {
    file = "test.lua",
    checker = "luacheck",
    passed = false,
    exit_code = 1,
    issues = {
      { line = 1, column = 1, code = "W211", message = "unused" }
    }
  }
  cosmo.Barf(path.join(test_dir, "test.luacheck.ok"), "return " .. cosmo.EncodeLua(result) .. "\n")

  local exit_code = luacheck_module.main({ "report", test_dir })
  lu.assertEquals(exit_code, 1)
end

function TestExitCode:test_check_fails_on_nonzero_exit_code()
  local test_file = path.join(TEST_TMPDIR, "exit_code_test.lua")
  local output_file = path.join(TEST_TMPDIR, "exit_code_test.lua.luacheck.ok")
  local fake_luacheck = path.join(TEST_TMPDIR, "fake-luacheck.sh")

  cosmo.Barf(test_file, 'local x = 1\n')
  cosmo.Barf(fake_luacheck, '#!/bin/sh\nexit 127\n')
  unix.chmod(fake_luacheck, tonumber("755", 8))

  local exit_code = luacheck_module.main({ test_file, output_file, fake_luacheck })

  lu.assertEquals(exit_code, 1)

  local chunk = loadfile(output_file)
  lu.assertNotNil(chunk)
  local result = chunk()
  lu.assertEquals(result.exit_code, 127)
  lu.assertFalse(result.passed)
end
