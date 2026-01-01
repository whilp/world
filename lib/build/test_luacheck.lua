local lu = require("luaunit")
local cosmo = require("cosmo")
local path = require("cosmo.path")
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
