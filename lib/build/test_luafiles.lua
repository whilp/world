local lu = require("luaunit")
local cosmo = require("cosmo")
local manifest = require("build.manifest")

local luafiles_path = TEST_ARGS[1]
local luatests_path = TEST_ARGS[2]
local git_path = TEST_ARGS[3]

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
local luafiles = read_lines(luafiles_path)
local luafiles_set = to_set(luafiles)
local luatests = read_lines(luatests_path)
local luatests_set = to_set(luatests)

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
  local git_set = to_set(git_files)
  for _, path in ipairs(luafiles) do
    lu.assertTrue(git_set[path], "not in git: " .. path)
  end
end

TestLuaExtension = {}

function TestLuaExtension:test_lua_extension_files_included()
  for _, path in ipairs(git_files) do
    if path:match("%.lua$") then
      local dominated = manifest.detect_type(path) == "lua"
      if dominated and not luafiles_set[path] then
        local info = nil
        for f in manifest.files({ _git_output = git_content }) do
          if f.path == path then
            info = f
            break
          end
        end
        if info and info.is_check then
          lu.fail("lua file missing: " .. path)
        end
      end
    end
  end
end

TestShebangFiles = {}

function TestShebangFiles:test_lua_shebang_files_included()
  for f in manifest.files({ _git_output = git_content }) do
    if f.type == "lua" and f.is_check then
      lu.assertTrue(luafiles_set[f.path], "lua file missing: " .. f.path)
    end
  end
end

TestLuaFilesConsistency = {}

function TestLuaFilesConsistency:test_matches_manifest_find_lua_files()
  local expected = manifest.find_lua_files({ _git_output = git_content })
  lu.assertEquals(#luafiles, #expected, "file count mismatch")
  for i, path in ipairs(luafiles) do
    lu.assertEquals(path, expected[i], "mismatch at line " .. i)
  end
end

TestLuaFilesExclusions = {}

function TestLuaFilesExclusions:test_check_false_excluded()
  for f in manifest.files({ _git_output = git_content }) do
    if f.type == "lua" and not f.is_check then
      lu.assertNil(luafiles_set[f.path], "should be excluded: " .. f.path)
    end
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
  local git_set = to_set(git_files)
  for _, path in ipairs(luatests) do
    lu.assertTrue(git_set[path], "not in git: " .. path)
  end
end

function TestLuaTestsBasic:test_all_tests_are_lua_files()
  for _, path in ipairs(luatests) do
    lu.assertTrue(luafiles_set[path], "test not in luafiles: " .. path)
  end
end

TestLuaTestsInclusion = {}

function TestLuaTestsInclusion:test_test_prefix_files_included()
  for _, path in ipairs(luafiles) do
    local basename = path:match("([^/]+)$")
    if basename and basename:match("^test_") then
      for f in manifest.files({ _git_output = git_content }) do
        if f.path == path and f.is_test and f.is_check then
          lu.assertTrue(luatests_set[path], "test file missing: " .. path)
          break
        end
      end
    end
  end
end

function TestLuaTestsInclusion:test_test_suffix_files_included()
  for _, path in ipairs(luafiles) do
    if path:match("/test%.lua$") then
      for f in manifest.files({ _git_output = git_content }) do
        if f.path == path and f.is_test and f.is_check then
          lu.assertTrue(luatests_set[path], "test file missing: " .. path)
          break
        end
      end
    end
  end
end

TestLuaTestsConsistency = {}

function TestLuaTestsConsistency:test_matches_manifest_find_lua_tests()
  local expected = manifest.find_lua_tests({ _git_output = git_content })
  lu.assertEquals(#luatests, #expected, "file count mismatch")
  for i, path in ipairs(luatests) do
    lu.assertEquals(path, expected[i], "mismatch at line " .. i)
  end
end

TestLuaTestsExclusions = {}

function TestLuaTestsExclusions:test_test_false_excluded()
  for f in manifest.files({ _git_output = git_content }) do
    if f.type == "lua" and f.is_check and not f.is_test then
      lu.assertNil(luatests_set[f.path], "should be excluded: " .. f.path)
    end
  end
end

function TestLuaTestsExclusions:test_check_false_excluded()
  for f in manifest.files({ _git_output = git_content }) do
    if f.type == "lua" and not f.is_check then
      lu.assertNil(luatests_set[f.path], "should be excluded: " .. f.path)
    end
  end
end
