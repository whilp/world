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

function TestFilesIterator:test_returns_iterator()
  local iter = manifest.files({
    _git_output = mock_git_output,
    _detect_type = mock_detect_type,
  })
  lu.assertNotNil(iter)
  local first = iter()
  lu.assertNotNil(first)
  lu.assertNotNil(first.path)
end

function TestFilesIterator:test_file_has_attributes()
  local iter = manifest.files({
    type = "lua",
    _git_output = mock_git_output,
    _detect_type = mock_detect_type,
  })
  local file = iter()
  lu.assertNotNil(file.path)
  lu.assertEquals(file.type, "lua")
  lu.assertNotNil(file.is_test)
end

function TestFilesIterator:test_filter_by_type()
  local iter = manifest.files({
    type = "lua",
    _git_output = mock_git_output,
    _detect_type = mock_detect_type,
  })
  for file in iter do
    lu.assertEquals(file.type, "lua", "should only return lua files: " .. file.path)
  end
end

function TestFilesIterator:test_filter_by_test()
  local iter = manifest.files({
    type = "lua",
    is_test = true,
    _git_output = mock_git_output,
    _detect_type = mock_detect_type,
  })
  for file in iter do
    lu.assertTrue(file.is_test, "should only return test files: " .. file.path)
  end
end

function TestFilesIterator:test_excludes_hammerspoon()
  local iter = manifest.files({
    _git_output = mock_git_output,
    _detect_type = mock_detect_type,
  })
  for file in iter do
    lu.assertNil(file.path:match("^%.config/hammerspoon/"), "should exclude .config/hammerspoon: " .. file.path)
  end
end

function TestFilesIterator:test_excludes_nvim()
  local iter = manifest.files({
    _git_output = mock_git_output,
    _detect_type = mock_detect_type,
  })
  for file in iter do
    lu.assertNil(file.path:match("^%.config/nvim/"), "should exclude .config/nvim: " .. file.path)
  end
end

function TestFilesIterator:test_excludes_output_dir()
  local iter = manifest.files({
    _git_output = mock_git_output,
    _detect_type = mock_detect_type,
  })
  for file in iter do
    lu.assertNil(file.path:match("^o/"), "should exclude o/ directory: " .. file.path)
  end
end

function TestFilesIterator:test_collect()
  local results = manifest.collect(manifest.files({
    type = "lua",
    is_test = true,
    _git_output = mock_git_output,
    _detect_type = mock_detect_type,
  }))
  lu.assertTrue(#results > 0)
  for i = 2, #results do
    lu.assertTrue(results[i-1].path <= results[i].path, "should be sorted")
  end
end

function TestFilesIterator:test_finds_test_files()
  local results = manifest.collect(manifest.files({
    type = "lua",
    is_test = true,
    _git_output = mock_git_output,
    _detect_type = mock_detect_type,
  }))
  local paths = {}
  for _, r in ipairs(results) do
    paths[r.path] = true
  end
  lu.assertTrue(paths["lib/build/test_manifest.lua"], "should find test_manifest.lua")
  lu.assertTrue(paths["lib/aerosnap/test.lua"], "should find test.lua")
  lu.assertTrue(paths["3p/argparse/test.lua"], "should find 3p test.lua")
end

function TestFilesIterator:test_finds_lua_files()
  local results = manifest.collect(manifest.files({
    type = "lua",
    _git_output = mock_git_output,
    _detect_type = mock_detect_type,
  }))
  local paths = {}
  for _, r in ipairs(results) do
    paths[r.path] = true
  end
  lu.assertTrue(paths["lib/build/manifest.lua"], "should find manifest.lua")
  lu.assertTrue(paths["lib/aerosnap/init.lua"], "should find init.lua")
end
