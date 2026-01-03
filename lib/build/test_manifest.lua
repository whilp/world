local lu = require("luaunit")
local manifest = require("build.manifest")

local mock_git_output = table.concat({
  "lib/build/manifest.lua",
  "lib/build/test_manifest.lua",
  "lib/aerosnap/test.lua",
  "lib/aerosnap/init.lua",
  "3p/argparse/test.lua",
  ".config/hammerspoon/init.lua",
  ".config/nvim/init.lua",
  "o/any/test.lua",
  "script.sh",
  "README.md",
}, "\0") .. "\0"

local function mock_detect_type(path)
  if path:match("%.lua$") then return "lua" end
  if path:match("%.sh$") then return "shell" end
  if path:match("%.md$") then return "markdown" end
  return nil
end

TestDetectType = {}

function TestDetectType:test_lua_extension()
  lu.assertEquals(manifest.detect_type("foo/bar.lua"), "lua")
end

function TestDetectType:test_shell_extension()
  lu.assertEquals(manifest.detect_type("script.sh"), "shell")
end

function TestDetectType:test_python_extension()
  lu.assertEquals(manifest.detect_type("script.py"), "python")
end

function TestDetectType:test_make_extension()
  lu.assertEquals(manifest.detect_type("lib/cook.mk"), "make")
end

function TestDetectType:test_yaml_extension()
  lu.assertEquals(manifest.detect_type("config.yml"), "yaml")
  lu.assertEquals(manifest.detect_type("config.yaml"), "yaml")
end

function TestDetectType:test_json_extension()
  lu.assertEquals(manifest.detect_type("data.json"), "json")
end

function TestDetectType:test_unknown_extension()
  lu.assertNil(manifest.detect_type("README"))
end

TestIsTestFile = {}

function TestIsTestFile:test_test_prefix()
  lu.assertTrue(manifest.is_test_file("lib/build/test_manifest.lua"))
  lu.assertTrue(manifest.is_test_file("test_foo.lua"))
end

function TestIsTestFile:test_test_suffix()
  lu.assertTrue(manifest.is_test_file("lib/foo/test.lua"))
  lu.assertTrue(manifest.is_test_file("3p/argparse/test.lua"))
end

function TestIsTestFile:test_not_test()
  lu.assertFalse(manifest.is_test_file("lib/build/manifest.lua"))
  lu.assertFalse(manifest.is_test_file("lib/testing/init.lua"))
end

TestIsExcluded = {}

function TestIsExcluded:test_hammerspoon_excluded()
  lu.assertTrue(manifest.is_excluded(".config/hammerspoon/init.lua", manifest.default_excluded))
end

function TestIsExcluded:test_nvim_excluded()
  lu.assertTrue(manifest.is_excluded(".config/nvim/init.lua", manifest.default_excluded))
end

function TestIsExcluded:test_output_dir_excluded()
  lu.assertTrue(manifest.is_excluded("o/any/test.lua", manifest.default_excluded))
end

function TestIsExcluded:test_lib_not_excluded()
  lu.assertFalse(manifest.is_excluded("lib/build/manifest.lua", manifest.default_excluded))
end

TestGitFilesIter = {}

function TestGitFilesIter:test_parses_null_delimited()
  local iter = manifest.git_files_iter("a.lua\0b.lua\0c.lua\0")
  lu.assertEquals(iter(), "a.lua")
  lu.assertEquals(iter(), "b.lua")
  lu.assertEquals(iter(), "c.lua")
  lu.assertNil(iter())
end

function TestGitFilesIter:test_empty_string()
  local iter = manifest.git_files_iter("")
  lu.assertNil(iter())
end

function TestGitFilesIter:test_single_file()
  local iter = manifest.git_files_iter("file.lua\0")
  lu.assertEquals(iter(), "file.lua")
  lu.assertNil(iter())
end

TestFilesIterator = {}

function TestFilesIterator:test_returns_all_files()
  local iter = manifest.files({
    _git_output = mock_git_output,
    _detect_type = mock_detect_type,
  })
  local count = 0
  for _ in iter do
    count = count + 1
  end
  lu.assertEquals(count, 10)
end

function TestFilesIterator:test_file_has_attributes()
  local iter = manifest.files({
    _git_output = mock_git_output,
    _detect_type = mock_detect_type,
  })
  local file = iter()
  lu.assertNotNil(file.path)
  lu.assertNotNil(file.type)
  lu.assertNotNil(file.is_test)
end

function TestFilesIterator:test_caller_can_filter_by_type()
  local count = 0
  for f in manifest.files({_git_output = mock_git_output, _detect_type = mock_detect_type}) do
    if f.type == "lua" then
      count = count + 1
    end
  end
  lu.assertEquals(count, 8)
end

function TestFilesIterator:test_caller_can_filter_by_test()
  local count = 0
  for f in manifest.files({_git_output = mock_git_output, _detect_type = mock_detect_type}) do
    if f.is_test then
      count = count + 1
    end
  end
  lu.assertEquals(count, 4)
end

function TestFilesIterator:test_caller_can_filter_excluded()
  local count = 0
  for f in manifest.files({_git_output = mock_git_output, _detect_type = mock_detect_type}) do
    if not manifest.is_excluded(f.path, manifest.default_excluded) then
      count = count + 1
    end
  end
  lu.assertEquals(count, 7)
end

TestFindLuaFiles = {}

function TestFindLuaFiles:test_finds_lua_excludes_config()
  local paths = manifest.find_lua_files({
    _git_output = mock_git_output,
    _detect_type = mock_detect_type,
  })
  lu.assertEquals(#paths, 5)
  for _, p in ipairs(paths) do
    lu.assertNil(p:match("^%.config/"), "should exclude .config: " .. p)
    lu.assertNil(p:match("^o/"), "should exclude o/: " .. p)
  end
end

function TestFindLuaFiles:test_sorted()
  local paths = manifest.find_lua_files({
    _git_output = mock_git_output,
    _detect_type = mock_detect_type,
  })
  for i = 2, #paths do
    lu.assertTrue(paths[i-1] <= paths[i], "should be sorted")
  end
end

TestFindLuaTests = {}

function TestFindLuaTests:test_finds_tests_excludes_config()
  local paths = manifest.find_lua_tests({
    _git_output = mock_git_output,
    _detect_type = mock_detect_type,
  })
  lu.assertEquals(#paths, 3)
  for _, p in ipairs(paths) do
    lu.assertTrue(manifest.is_test_file(p), "should be test: " .. p)
  end
end

function TestFindLuaTests:test_sorted()
  local paths = manifest.find_lua_tests({
    _git_output = mock_git_output,
    _detect_type = mock_detect_type,
  })
  for i = 2, #paths do
    lu.assertTrue(paths[i-1] <= paths[i], "should be sorted")
  end
end
