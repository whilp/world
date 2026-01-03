local lu = require("luaunit")
local cosmo = require("cosmo")

local git_path = TEST_ARGS[1]
local luafiles_path = TEST_ARGS[2]
local luatests_path = TEST_ARGS[3]

local function parse_null_separated(content)
  local files = {}
  for file in content:gmatch("([^\0]+)") do
    files[#files + 1] = file
  end
  return files
end

local function read_lines(filepath)
  local content = cosmo.Slurp(filepath)
  if not content then
    return nil
  end
  local lines = {}
  for line in content:gmatch("([^\n]+)") do
    if line ~= "" then
      lines[#lines + 1] = line
    end
  end
  return lines
end

local function to_set(list)
  local set = {}
  for _, v in ipairs(list) do
    set[v] = true
  end
  return set
end

local git_content = cosmo.Slurp(git_path)
local git_files = parse_null_separated(git_content)
local git_set = to_set(git_files)
local luafiles = read_lines(luafiles_path)
local luafiles_set = to_set(luafiles)
local luatests = read_lines(luatests_path)

-- Lua files tests

TestLuaFilesBasic = {}

function TestLuaFilesBasic:test_luafiles_not_empty()
  lu.assertTrue(#luafiles > 0, "luafiles should not be empty")
end

function TestLuaFilesBasic:test_luafiles_sorted()
  for i = 2, #luafiles do
    lu.assertTrue(luafiles[i-1] < luafiles[i],
      string.format("not sorted: %s >= %s", luafiles[i-1], luafiles[i]))
  end
end

function TestLuaFilesBasic:test_no_duplicates()
  local seen = {}
  for _, path in ipairs(luafiles) do
    lu.assertNil(seen[path], "duplicate: " .. path)
    seen[path] = true
  end
end

function TestLuaFilesBasic:test_all_files_exist_in_git()
  for _, path in ipairs(luafiles) do
    lu.assertTrue(git_set[path], "not in git: " .. path)
  end
end

function TestLuaFilesBasic:test_all_files_are_lua()
  for _, path in ipairs(luafiles) do
    local ext = path:match("%.([^.]+)$")
    local is_lua_ext = ext == "lua"
    local is_bin = path:match("^%.local/bin/") ~= nil
      or path:match("^3p/luacheck/luacheck$") ~= nil
      or path:match("^%.config/voyager/fetch$") ~= nil
    lu.assertTrue(is_lua_ext or is_bin, "not lua: " .. path)
  end
end

-- Lua tests tests

TestLuaTestsBasic = {}

function TestLuaTestsBasic:test_luatests_not_empty()
  lu.assertTrue(#luatests > 0, "luatests should not be empty")
end

function TestLuaTestsBasic:test_luatests_sorted()
  for i = 2, #luatests do
    lu.assertTrue(luatests[i-1] < luatests[i],
      string.format("not sorted: %s >= %s", luatests[i-1], luatests[i]))
  end
end

function TestLuaTestsBasic:test_no_duplicates()
  local seen = {}
  for _, path in ipairs(luatests) do
    lu.assertNil(seen[path], "duplicate: " .. path)
    seen[path] = true
  end
end

function TestLuaTestsBasic:test_all_tests_exist_in_git()
  for _, path in ipairs(luatests) do
    lu.assertTrue(git_set[path], "not in git: " .. path)
  end
end

function TestLuaTestsBasic:test_all_tests_are_lua_files()
  for _, path in ipairs(luatests) do
    lu.assertTrue(luafiles_set[path], "test not in luafiles: " .. path)
  end
end

function TestLuaTestsBasic:test_all_tests_have_test_pattern()
  for _, path in ipairs(luatests) do
    local basename = path:match("([^/]+)$")
    local is_test = basename:match("^test_") ~= nil or basename == "test.lua"
    lu.assertTrue(is_test, "not a test pattern: " .. path)
  end
end
