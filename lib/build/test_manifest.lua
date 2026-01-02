local lu = require("luaunit")
local manifest = require("build.manifest")
local unix = require("cosmo.unix")
local path = require("cosmo.path")
local cosmo = require("cosmo")

TestHasLuaShebang = {}

function TestHasLuaShebang:test_lua_shebang()
  local filepath = path.join(TEST_TMPDIR, "script.lua")
  cosmo.Barf(filepath, "#!/usr/bin/env lua\nprint('hello')\n")

  local result = manifest.has_lua_shebang(filepath)
  lu.assertTrue(result)
end

function TestHasLuaShebang:test_lua5_4_shebang()
  local filepath = path.join(TEST_TMPDIR, "script")
  cosmo.Barf(filepath, "#!/usr/bin/lua5.4\nprint('hello')\n")

  local result = manifest.has_lua_shebang(filepath)
  lu.assertTrue(result)
end

function TestHasLuaShebang:test_luajit_shebang()
  local filepath = path.join(TEST_TMPDIR, "script")
  cosmo.Barf(filepath, "#!/usr/bin/env luajit\nprint('hello')\n")

  local result = manifest.has_lua_shebang(filepath)
  lu.assertTrue(result)
end

function TestHasLuaShebang:test_no_shebang()
  local filepath = path.join(TEST_TMPDIR, "script")
  cosmo.Barf(filepath, "print('hello')\n")

  local result = manifest.has_lua_shebang(filepath)
  lu.assertFalse(result)
end

function TestHasLuaShebang:test_bash_shebang()
  local filepath = path.join(TEST_TMPDIR, "script.sh")
  cosmo.Barf(filepath, "#!/bin/bash\necho 'hello'\n")

  local result = manifest.has_lua_shebang(filepath)
  lu.assertFalse(result)
end

function TestHasLuaShebang:test_python_shebang()
  local filepath = path.join(TEST_TMPDIR, "script.py")
  cosmo.Barf(filepath, "#!/usr/bin/env python3\nprint('hello')\n")

  local result = manifest.has_lua_shebang(filepath)
  lu.assertFalse(result)
end

function TestHasLuaShebang:test_empty_file()
  local filepath = path.join(TEST_TMPDIR, "empty")
  cosmo.Barf(filepath, "")

  local result = manifest.has_lua_shebang(filepath)
  lu.assertFalse(result)
end

function TestHasLuaShebang:test_nonexistent_file()
  local result = manifest.has_lua_shebang("/nonexistent/file")
  lu.assertFalse(result)
end

function TestHasLuaShebang:test_shebang_with_space()
  local filepath = path.join(TEST_TMPDIR, "script")
  cosmo.Barf(filepath, "#! /usr/bin/env lua\nprint('hello')\n")

  local result = manifest.has_lua_shebang(filepath)
  lu.assertTrue(result)
end

function TestHasLuaShebang:test_shebang_with_args()
  local filepath = path.join(TEST_TMPDIR, "script")
  cosmo.Barf(filepath, "#!/usr/bin/lua -e\nprint('hello')\n")

  local result = manifest.has_lua_shebang(filepath)
  lu.assertTrue(result)
end

function TestHasLuaShebang:test_not_first_line()
  local filepath = path.join(TEST_TMPDIR, "script")
  cosmo.Barf(filepath, "\n#!/usr/bin/env lua\nprint('hello')\n")

  local result = manifest.has_lua_shebang(filepath)
  lu.assertFalse(result)
end

TestFindLuaFiles = {}

function TestFindLuaFiles:test_basic_search()
  local files, err = manifest.find_lua_files()
  lu.assertNotNil(files, err)
  lu.assertTrue(#files > 0)
end

function TestFindLuaFiles:test_includes_lua_extension()
  local files, err = manifest.find_lua_files()
  lu.assertNotNil(files, err)

  local found = false
  for _, file in ipairs(files) do
    if file == "lib/build/action.lua" then
      found = true
      break
    end
  end
  lu.assertTrue(found, "should include lib/build/action.lua")
end

function TestFindLuaFiles:test_excludes_hammerspoon()
  local files, err = manifest.find_lua_files()
  lu.assertNotNil(files, err)

  for _, file in ipairs(files) do
    lu.assertNil(file:match("^%.config/hammerspoon/"), "should exclude .config/hammerspoon files: " .. file)
  end
end

function TestFindLuaFiles:test_excludes_nvim()
  local files, err = manifest.find_lua_files()
  lu.assertNotNil(files, err)

  for _, file in ipairs(files) do
    lu.assertNil(file:match("^%.config/nvim/"), "should exclude .config/nvim files: " .. file)
  end
end

function TestFindLuaFiles:test_excludes_output_dir()
  local files, err = manifest.find_lua_files()
  lu.assertNotNil(files, err)

  for _, file in ipairs(files) do
    lu.assertNil(file:match("^o/"), "should exclude o/ directory: " .. file)
  end
end

function TestFindLuaFiles:test_custom_exclusions()
  local files, err = manifest.find_lua_files({
    excluded_patterns = {"^3p/"}
  })
  lu.assertNotNil(files, err)

  for _, file in ipairs(files) do
    lu.assertNil(file:match("^3p/"), "should exclude 3p/ directory: " .. file)
  end
end

function TestFindLuaFiles:test_sorted_output()
  local files, err = manifest.find_lua_files()
  lu.assertNotNil(files, err)

  for i = 2, #files do
    lu.assertTrue(files[i-1] <= files[i], "files should be sorted")
  end
end
