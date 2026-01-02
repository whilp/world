local lu = require("luaunit")
local manifest = require("build.manifest")

TestHasLuaShebang = {}

function TestHasLuaShebang:test_lua_shebang()
  lu.assertTrue(manifest.has_lua_shebang("#!/usr/bin/env lua\nprint('hello')"))
end

function TestHasLuaShebang:test_shebang_with_space()
  lu.assertTrue(manifest.has_lua_shebang("#! /usr/bin/lua\nprint('hello')"))
end

function TestHasLuaShebang:test_no_shebang()
  lu.assertFalse(manifest.has_lua_shebang("print('hello')"))
end

function TestHasLuaShebang:test_bash_shebang()
  lu.assertFalse(manifest.has_lua_shebang("#!/bin/bash\necho 'hello'"))
end

function TestHasLuaShebang:test_not_first_line()
  lu.assertFalse(manifest.has_lua_shebang("\n#!/usr/bin/env lua"))
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
