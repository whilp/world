-- teal ignore: type annotations needed

local common = require("checker.common")
local path = require("cosmo.path")
local unix = require("cosmo.unix")
local cosmo = require("cosmo")

local TEST_TMPDIR = os.getenv("TEST_TMPDIR")

local function test_format_output()
  local output = common.format_output("pass", nil, "", "")
  assert(output:match("^pass\n"), "should format status without message")
  assert(output:match("## stdout"), "should include stdout section")
  assert(output:match("## stderr"), "should include stderr section")

  local output2 = common.format_output("fail", "3 issues", "", "error details")
  assert(output2:match("^fail: 3 issues"), "should format status with message")
  assert(output2:match("## stderr\n\nerror details"), "should include stderr content")
end

local function test_parse_result()
  local content = [[pass

## stdout

test output
## stderr

]]
  local result = common.parse_result(content)
  assert(result.status == "pass", "should parse status")
  assert(result.message == nil, "should parse nil message")
  assert(result.stdout == "test output", "should parse stdout")
  assert(result.stderr == "", "should parse empty stderr")

  local content2 = [[fail: 2 issues

## stdout


## stderr

file.lua:10:5: error message]]
  local result2 = common.parse_result(content2)
  assert(result2.status == "fail", "should parse fail status")
  assert(result2.message == "2 issues", "should parse message")
  assert(result2.stderr:match("error message"), "should parse stderr")
end

local function test_strip_prefix()
  local old_test_o = os.getenv("TEST_O")

  os.getenv = function(name)
    if name == "TEST_O" then
      return "o"
    end
    return old_test_o
  end

  local result = common.strip_prefix("o/lib/test.lua")
  assert(result == "lib/test.lua", "should strip o/ prefix")

  local result2 = common.strip_prefix("lib/test.lua")
  assert(result2 == "lib/test.lua", "should not strip if no prefix match")

  os.getenv = function(name)
    if name == "TEST_O" then
      return nil
    end
  end

  local result3 = common.strip_prefix("o/lib/test.lua")
  assert(result3 == "o/lib/test.lua", "should not strip if TEST_O not set")

  os.getenv = function(name)
    return old_test_o
  end
end

local function test_status_icons()
  local icons = common.status_icons()
  assert(icons.pass == "✓", "should have pass icon")
  assert(icons.fail == "✗", "should have fail icon")
  assert(icons.skip == "→", "should have skip icon")
  assert(icons.ignore == "○", "should have ignore icon")
end

local function test_has_extension()
  local extensions = { [".lua"] = true, [".tl"] = true }
  assert(common.has_extension("test.lua", extensions), "should match .lua extension")
  assert(common.has_extension("test.tl", extensions), "should match .tl extension")
  assert(not common.has_extension("test.py", extensions), "should not match .py extension")
  assert(not common.has_extension("test", extensions), "should not match no extension")
end

local function test_check_first_lines()
  local test_file = path.join(TEST_TMPDIR, "test.lua")

  cosmo.Barf(test_file, [[#!/usr/bin/env lua
-- some comment
-- checker ignore: test reason
local x = 1
]])

  local has_shebang, skip_reason = common.check_first_lines(test_file, {
    shebangs = { lua = true },
    ignore = "checker%s+ignore%s*:?%s*(.*)"
  })
  assert(has_shebang == true, "should detect shebang")
  assert(skip_reason == "test reason", "should detect skip reason")

  local test_file2 = path.join(TEST_TMPDIR, "test2.lua")
  cosmo.Barf(test_file2, [[-- regular file
local x = 1
]])

  local has_shebang2, skip_reason2 = common.check_first_lines(test_file2, {
    shebangs = { lua = true },
    ignore = "checker%s+ignore%s*:?%s*(.*)"
  })
  assert(has_shebang2 == false, "should not detect shebang")
  assert(skip_reason2 == nil, "should not detect skip reason")

  local test_file3 = path.join(TEST_TMPDIR, "test3.lua")
  cosmo.Barf(test_file3, [[#!/usr/bin/env lua
-- checker ignore
local x = 1
]])

  local has_shebang3, skip_reason3 = common.check_first_lines(test_file3, {
    shebangs = { lua = true },
    ignore = "checker%s+ignore%s*:?%s*(.*)"
  })
  assert(has_shebang3 == true, "should detect shebang")
  assert(skip_reason3 == "directive", "should use default skip reason")
end

local function test_categorize_results()
  local results_list = {
    { status = "pass", name = "test1" },
    { status = "fail", name = "test2" },
    { status = "pass", name = "test3" },
    { status = "skip", name = "test4" },
    { status = "ignore", name = "test5" },
  }

  local results = common.categorize_results(results_list)
  assert(#results.pass == 2, "should categorize pass results")
  assert(#results.fail == 1, "should categorize fail results")
  assert(#results.skip == 1, "should categorize skip results")
  assert(#results.ignore == 1, "should categorize ignore results")
end

test_format_output()
test_parse_result()
test_strip_prefix()
test_status_icons()
test_has_extension()
test_check_first_lines()
test_categorize_results()
