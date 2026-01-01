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

TestWalk = {}

function TestWalk:test_walk_finds_lua_files()
  local test_dir = path.join(TEST_TMPDIR, "walk_test")
  unix.makedirs(test_dir)
  cosmo.Barf(path.join(test_dir, "file1.lua"), "")
  cosmo.Barf(path.join(test_dir, "file2.lua"), "")
  cosmo.Barf(path.join(test_dir, "file.txt"), "")

  local results = luacheck_module.walk(test_dir, "%.lua$")
  lu.assertEquals(#results, 2)
end

function TestWalk:test_walk_finds_nested_files()
  local test_dir = path.join(TEST_TMPDIR, "walk_nested")
  local sub_dir = path.join(test_dir, "subdir")
  unix.makedirs(sub_dir)
  cosmo.Barf(path.join(test_dir, "top.lua"), "")
  cosmo.Barf(path.join(sub_dir, "nested.lua"), "")

  local results = luacheck_module.walk(test_dir, "%.lua$")
  lu.assertEquals(#results, 2)
end

function TestWalk:test_walk_empty_directory()
  local test_dir = path.join(TEST_TMPDIR, "walk_empty")
  unix.makedirs(test_dir)

  local results = luacheck_module.walk(test_dir, "%.lua$")
  lu.assertEquals(#results, 0)
end

function TestWalk:test_walk_no_matches()
  local test_dir = path.join(TEST_TMPDIR, "walk_no_match")
  unix.makedirs(test_dir)
  cosmo.Barf(path.join(test_dir, "file.txt"), "")

  local results = luacheck_module.walk(test_dir, "%.lua$")
  lu.assertEquals(#results, 0)
end

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
