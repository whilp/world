#!/usr/bin/env run-test.lua
-- teal ignore: test file
-- test cosmic script argument passing (varargs and arg table)

local unix = require("cosmo.unix")
local path = require("cosmo.path")
local spawn = require("cosmic.spawn")
local cosmo = require("cosmo")

local cosmic = path.join(os.getenv("TEST_BIN"), "cosmic")
local tmpdir = os.getenv("TEST_TMPDIR")

-- Test script that uses only varargs (...)
local function test_varargs_only()
  local script = path.join(tmpdir, "varargs.lua")
  cosmo.Barf(script, [[
local args = {...}
print("varargs_count=" .. #args)
for i, v in ipairs(args) do
  print("vararg[" .. i .. "]=" .. tostring(v))
end
]])

  local ok, out = spawn({cosmic, script, "foo", "bar", "baz"}):read()
  assert(ok, "cosmic should succeed")
  assert(out:find("varargs_count=3"), "should have 3 varargs")
  assert(out:find("vararg%[1%]=foo"), "vararg[1] should be foo")
  assert(out:find("vararg%[2%]=bar"), "vararg[2] should be bar")
  assert(out:find("vararg%[3%]=baz"), "vararg[3] should be baz")
end
test_varargs_only()

-- Test script that uses only global arg table
local function test_arg_table_only()
  local script = path.join(tmpdir, "argtable.lua")
  cosmo.Barf(script, [[
print("arg_count=" .. #arg)
print("arg[0]=" .. tostring(arg[0]))
for i = 1, #arg do
  print("arg[" .. i .. "]=" .. tostring(arg[i]))
end
]])

  local ok, out = spawn({cosmic, script, "alpha", "beta"}):read()
  assert(ok, "cosmic should succeed")
  assert(out:find("arg_count=2"), "should have 2 args")
  assert(out:find("arg%[0%]=" .. script:gsub("[%-%.]", "%%%1")), "arg[0] should be script path")
  assert(out:find("arg%[1%]=alpha"), "arg[1] should be alpha")
  assert(out:find("arg%[2%]=beta"), "arg[2] should be beta")
end
test_arg_table_only()

-- Test that both varargs and arg table work together and match
local function test_varargs_and_arg_table()
  local script = path.join(tmpdir, "both.lua")
  cosmo.Barf(script, [[
local varargs = {...}
print("varargs_count=" .. #varargs)
print("arg_count=" .. #arg)
print("match=" .. tostring(#varargs == #arg))
for i = 1, #arg do
  print("match[" .. i .. "]=" .. tostring(arg[i] == varargs[i]))
end
]])

  local ok, out = spawn({cosmic, script, "x", "y", "z"}):read()
  assert(ok, "cosmic should succeed")
  assert(out:find("varargs_count=3"), "varargs should have 3 elements")
  assert(out:find("arg_count=3"), "arg should have 3 elements")
  assert(out:find("match=true"), "varargs and arg counts should match")
  assert(out:find("match%[1%]=true"), "arg[1] should match varargs[1]")
  assert(out:find("match%[2%]=true"), "arg[2] should match varargs[2]")
  assert(out:find("match%[3%]=true"), "arg[3] should match varargs[3]")
end
test_varargs_and_arg_table()

-- Test the common pattern: function main(...) os.exit(main(...))
local function test_main_function_with_varargs()
  local script = path.join(tmpdir, "main_varargs.lua")
  cosmo.Barf(script, [[
local function main(...)
  local args = {...}
  print("main_varargs_count=" .. #args)
  for i, v in ipairs(args) do
    print("main_arg[" .. i .. "]=" .. tostring(v))
  end
  return 0
end

os.exit(main(...))
]])

  local ok, out = spawn({cosmic, script, "one", "two"}):read()
  assert(ok, "cosmic should succeed")
  assert(out:find("main_varargs_count=2"), "main should receive 2 varargs")
  assert(out:find("main_arg%[1%]=one"), "main_arg[1] should be one")
  assert(out:find("main_arg%[2%]=two"), "main_arg[2] should be two")
end
test_main_function_with_varargs()

-- Test function main(param) with single optional parameter
local function test_main_function_with_single_param()
  local script = path.join(tmpdir, "main_param.lua")
  cosmo.Barf(script, [[
local function main(dir)
  dir = dir or "default"
  print("param=" .. dir)
  return 0
end

os.exit(main(...))
]])

  -- Test with argument
  local ok, out = spawn({cosmic, script, "custom"}):read()
  assert(ok, "cosmic should succeed")
  assert(out:find("param=custom"), "param should be custom")

  -- Test with no argument (should use default)
  ok, out = spawn({cosmic, script}):read()
  assert(ok, "cosmic should succeed")
  assert(out:find("param=default"), "param should be default")
end
test_main_function_with_single_param()

-- Test script with no arguments
local function test_no_arguments()
  local script = path.join(tmpdir, "noargs.lua")
  cosmo.Barf(script, [[
local args = {...}
print("varargs_count=" .. #args)
print("arg_count=" .. #arg)
print("arg[0]=" .. tostring(arg[0]))
]])

  local ok, out = spawn({cosmic, script}):read()
  assert(ok, "cosmic should succeed")
  assert(out:find("varargs_count=0"), "varargs should be empty")
  assert(out:find("arg_count=0"), "arg should be empty")
  assert(out:find("arg%[0%]="), "arg[0] should be set")
end
test_no_arguments()

-- Test arguments containing spaces
local function test_arguments_with_spaces()
  local script = path.join(tmpdir, "spaces.lua")
  cosmo.Barf(script, [[
local args = {...}
for i, v in ipairs(args) do
  print("arg[" .. i .. "]=" .. v)
end
]])

  local ok, out = spawn({cosmic, script, "hello world", "foo bar"}):read()
  assert(ok, "cosmic should succeed")
  assert(out:find("arg%[1%]=hello world"), "should preserve spaces in arg[1]")
  assert(out:find("arg%[2%]=foo bar"), "should preserve spaces in arg[2]")
end
test_arguments_with_spaces()

-- Test arguments with special characters (use -- to separate from cosmic options)
local function test_arguments_with_special_chars()
  local script = path.join(tmpdir, "special.lua")
  cosmo.Barf(script, [[
local args = {...}
for i, v in ipairs(args) do
  print("arg[" .. i .. "]=" .. v)
end
]])

  local ok, out = spawn({cosmic, script, "--", "--flag", "-x", "value=123"}):read()
  assert(ok, "cosmic should succeed")
  assert(out:find("arg%[1%]=%-%-flag"), "should handle --flag")
  assert(out:find("arg%[2%]=%-x"), "should handle -x")
  assert(out:find("arg%[3%]=value=123"), "should handle value=123")
end
test_arguments_with_special_chars()

-- Test that cosmo.is_main() still works with new varargs approach
local function test_cosmo_is_main_compatibility()
  local script = path.join(tmpdir, "is_main.lua")
  cosmo.Barf(script, [[
local cosmo = require("cosmo")

local function main(...)
  local args = {...}
  print("main_called=true")
  print("args_count=" .. #args)
  return 0
end

if cosmo.is_main() then
  os.exit(main(...))
end
]])

  local ok, out = spawn({cosmic, script, "test1", "test2"}):read()
  assert(ok, "cosmic should succeed")
  assert(out:find("main_called=true"), "main should be called")
  assert(out:find("args_count=2"), "main should receive 2 args")
end
test_cosmo_is_main_compatibility()
