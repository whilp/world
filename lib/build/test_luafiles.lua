local lu = require("luaunit")
local unix = require("cosmo.unix")
local manifest = require("build.manifest")

local function read_file(filepath)
  local fd = unix.open(filepath, unix.O_RDONLY)
  if not fd then
    return nil
  end
  local content = unix.read(fd, 1024 * 1024)
  unix.close(fd)
  return content
end

local function read_lines(filepath)
  local content = read_file(filepath)
  if not content then
    return nil
  end
  local lines = {}
  for line in content:gmatch("([^\n]+)") do
    if line ~= "" then
      table.insert(lines, line)
    end
  end
  return lines
end

TestLuaFilesTxt = {}

function TestLuaFilesTxt:test_matches_manifest_git()
  local txt_lines = read_lines("o/manifest/lua-files.txt")
  lu.assertNotNil(txt_lines, "o/manifest/lua-files.txt should exist")

  local git_content = read_file("o/manifest/git.txt")
  lu.assertNotNil(git_content, "o/manifest/git.txt should exist")

  local expected = manifest.find_lua_files({ _git_output = git_content })
  lu.assertEquals(#txt_lines, #expected, "file count mismatch")

  for i, path in ipairs(txt_lines) do
    lu.assertEquals(path, expected[i], "mismatch at line " .. i)
  end
end
