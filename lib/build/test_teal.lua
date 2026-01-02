local lu = require("luaunit")
local cosmo = require("cosmo")
local path = require("cosmo.path")
local unix = require("cosmo.unix")

local test_bin_dir = os.getenv("TEST_BIN_DIR")
if not test_bin_dir then
  local Makefile = io.open("Makefile"):read("*a")
  local current_platform = Makefile:match('current_platform := ([^\n]+)')
  if current_platform:match('%%') then
    current_platform = "linux-x86_64"
  end
  test_bin_dir = "o/" .. current_platform .. "/tl"
end

local lua_dist_dir = test_bin_dir:gsub("/tl$", "/lua")
local lua_dist = path.join(lua_dist_dir, "bin", "lua.dist")
local tl_bin = path.join(test_bin_dir, "bin", "tl")
local teal_script = "lib/build/teal.lua"

local teal_module = dofile(teal_script)

TestExitCode = {}

function TestExitCode:test_check_fails_on_nonzero_exit_code()
  local test_file = path.join(TEST_TMPDIR, "exit_code_test.lua")
  local output_file = path.join(TEST_TMPDIR, "exit_code_test.lua.teal.ok")
  local fake_tl = path.join(TEST_TMPDIR, "fake-tl.lua")
  local fake_lua = "o/any/lua/bin/lua"

  cosmo.Barf(test_file, 'local x = 1\n')
  cosmo.Barf(fake_tl, 'os.exit(127)\n')

  local exit_code = teal_module.main({ test_file, output_file, fake_tl, fake_lua })

  lu.assertEquals(exit_code, 1)

  local chunk = loadfile(output_file)
  lu.assertNotNil(chunk)
  local result = chunk()
  lu.assertEquals(result.exit_code, 127)
  lu.assertFalse(result.passed)
end

function TestExitCode:test_main_report_returns_0_when_all_pass()
  local test_dir = path.join(TEST_TMPDIR, "report_pass")
  unix.makedirs(test_dir)

  local result = {
    file = "test.lua",
    checker = "teal",
    passed = true,
    exit_code = 0,
    issues = {}
  }
  cosmo.Barf(path.join(test_dir, "test.teal.ok"), "return " .. cosmo.EncodeLua(result) .. "\n")

  local exit_code = teal_module.main({ "report", test_dir })
  lu.assertEquals(exit_code, 0)
end

function TestExitCode:test_main_report_returns_1_when_any_fail()
  local test_dir = path.join(TEST_TMPDIR, "report_fail")
  unix.makedirs(test_dir)

  local result = {
    file = "test.lua",
    checker = "teal",
    passed = false,
    exit_code = 1,
    issues = {
      {
        file = "test.lua",
        line = 1,
        column = 1,
        severity = "error",
        message = "type error"
      }
    }
  }
  cosmo.Barf(path.join(test_dir, "test.teal.ok"), "return " .. cosmo.EncodeLua(result) .. "\n")

  local exit_code = teal_module.main({ "report", test_dir })
  lu.assertEquals(exit_code, 1)
end

TestReport = {}

function TestReport:test_report_all_passed()
  local test_dir = path.join(TEST_TMPDIR, "report_all_pass")
  unix.makedirs(test_dir)

  local result1 = {
    file = "test1.lua",
    checker = "teal",
    passed = true,
    exit_code = 0,
    issues = {}
  }
  cosmo.Barf(path.join(test_dir, "test1.teal.ok"), "return " .. cosmo.EncodeLua(result1) .. "\n")

  local result2 = {
    file = "test2.lua",
    checker = "teal",
    passed = true,
    exit_code = 0,
    issues = {}
  }
  cosmo.Barf(path.join(test_dir, "test2.teal.ok"), "return " .. cosmo.EncodeLua(result2) .. "\n")

  local success = teal_module.report(test_dir)
  lu.assertTrue(success)
end

function TestReport:test_report_some_failed()
  local test_dir = path.join(TEST_TMPDIR, "report_some_fail")
  unix.makedirs(test_dir)

  local result1 = {
    file = "test1.lua",
    checker = "teal",
    passed = false,
    exit_code = 1,
    issues = {
      { file = "test1.lua", line = 10, column = 5, severity = "error", message = "type error" }
    }
  }
  cosmo.Barf(path.join(test_dir, "test1.teal.ok"), "return " .. cosmo.EncodeLua(result1) .. "\n")

  local success = teal_module.report(test_dir)
  lu.assertFalse(success)
end

function TestReport:test_report_counts_issues()
  local test_dir = path.join(TEST_TMPDIR, "report_counts_issues")
  unix.makedirs(test_dir)

  local result1 = {
    file = "test1.lua",
    checker = "teal",
    passed = false,
    exit_code = 1,
    issues = {
      { file = "test1.lua", line = 10, column = 5, severity = "error", message = "error 1" },
      { file = "test1.lua", line = 15, column = 3, severity = "warning", message = "warning 1" }
    }
  }
  cosmo.Barf(path.join(test_dir, "test1.teal.ok"), "return " .. cosmo.EncodeLua(result1) .. "\n")

  local result2 = {
    file = "test2.lua",
    checker = "teal",
    passed = false,
    exit_code = 1,
    issues = {
      { file = "test2.lua", line = 5, column = 2, severity = "error", message = "error 2" }
    }
  }
  cosmo.Barf(path.join(test_dir, "test2.teal.ok"), "return " .. cosmo.EncodeLua(result2) .. "\n")

  local success = teal_module.report(test_dir)
  lu.assertFalse(success)
end

function TestReport:test_report_empty_directory()
  local test_dir = path.join(TEST_TMPDIR, "report_empty_dir")
  unix.makedirs(test_dir)

  local success = teal_module.report(test_dir)
  lu.assertTrue(success)
end
