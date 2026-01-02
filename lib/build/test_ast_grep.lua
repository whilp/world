local lu = require("luaunit")
local cosmo = require("cosmo")
local path = require("cosmo.path")
local spawn = require("spawn").spawn

local ast_grep_bin = path.join(os.getenv("TEST_BIN_DIR"), "bin", "ast-grep")
local ast_grep_script = "lib/build/ast-grep.lua"
local lua_bin = "o/any/lua/bin/lua"

local function run_check(code, filename)
  local filepath = path.join(TEST_TMPDIR, filename)
  local output_path = path.join(TEST_TMPDIR, filename .. ".ast-grep.ok")
  cosmo.Barf(filepath, code)

  local handle = spawn({ lua_bin, ast_grep_script, filepath, output_path, ast_grep_bin })
  handle:wait()

  local chunk = loadfile(output_path)
  if not chunk then
    return nil, "failed to load output"
  end
  return chunk()
end

TestAstGrepCheck = {}

function TestAstGrepCheck:test_clean_code_passes()
  local result = run_check([[
local spawn = require("spawn").spawn
spawn({"ls"}):wait()
]], "clean.lua")
  lu.assertNotNil(result)
  lu.assertTrue(result.passed)
  lu.assertEquals(#result.issues, 0)
end

function TestAstGrepCheck:test_os_execute_detected()
  local result = run_check([[
os.execute("ls")
]], "os_exec.lua")
  lu.assertNotNil(result)
  lu.assertFalse(result.passed)
  lu.assertTrue(#result.issues > 0)
  lu.assertEquals(result.issues[1].rule_id, "avoid-os-execute")
end

function TestAstGrepCheck:test_io_popen_detected()
  local result = run_check([[
local handle = io.popen("ls")
handle:close()
]], "io_popen.lua")
  lu.assertNotNil(result)
  lu.assertFalse(result.passed)
  lu.assertTrue(#result.issues > 0)
  lu.assertEquals(result.issues[1].rule_id, "avoid-io-popen")
end

function TestAstGrepCheck:test_package_path_detected()
  local result = run_check([[
package.path = package.path .. ";/foo/?.lua"
]], "pkg_path.lua")
  lu.assertNotNil(result)
  lu.assertFalse(result.passed)
  lu.assertTrue(#result.issues > 0)
  lu.assertEquals(result.issues[1].rule_id, "avoid-package-path")
end

function TestAstGrepCheck:test_issues_have_location()
  local result = run_check([[
os.execute("ls")
]], "location.lua")
  lu.assertNotNil(result)
  lu.assertTrue(#result.issues > 0)
  local issue = result.issues[1]
  lu.assertNotNil(issue.line)
  lu.assertNotNil(issue.column)
  lu.assertNotNil(issue.rule_id)
  lu.assertNotNil(issue.severity)
end

function TestAstGrepCheck:test_result_structure()
  local result = run_check([[
local x = 1
return x
]], "structure.lua")
  lu.assertNotNil(result)
  lu.assertNotNil(result.file)
  lu.assertEquals(result.checker, "ast-grep")
  lu.assertIsBoolean(result.passed)
  lu.assertNotNil(result.exit_code)
  lu.assertIsTable(result.issues)
end

local ast_grep_module = dofile(ast_grep_script)

TestExitCode = {}

function TestExitCode:test_main_returns_0_on_pass()
  local test_file = path.join(TEST_TMPDIR, "pass.lua")
  local output_file = path.join(TEST_TMPDIR, "pass.lua.ast-grep.ok")
  cosmo.Barf(test_file, 'local spawn = require("spawn").spawn\nspawn({"ls"}):wait()\n')

  local exit_code = ast_grep_module.main({ test_file, output_file, ast_grep_bin })
  lu.assertEquals(exit_code, 0)
end

function TestExitCode:test_main_returns_1_on_fail()
  local test_file = path.join(TEST_TMPDIR, "fail.lua")
  local output_file = path.join(TEST_TMPDIR, "fail.lua.ast-grep.ok")
  cosmo.Barf(test_file, 'os.execute("ls")\n')

  local exit_code = ast_grep_module.main({ test_file, output_file, ast_grep_bin })
  lu.assertEquals(exit_code, 1)
end
