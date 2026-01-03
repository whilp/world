local lu = require("luaunit")
local cosmo = require("cosmo")
local manifest = require("build.manifest")

local luafiles_path = TEST_ARGS[1]
local git_path = TEST_ARGS[2]

local function read_lines(filepath)
  local content = cosmo.Slurp(filepath)
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
  local txt_lines = read_lines(luafiles_path)
  lu.assertNotNil(txt_lines, luafiles_path .. " should exist")

  local git_content = cosmo.Slurp(git_path)
  lu.assertNotNil(git_content, git_path .. " should exist")

  local expected = manifest.find_lua_files({ _git_output = git_content })
  lu.assertEquals(#txt_lines, #expected, "file count mismatch")

  for i, path in ipairs(txt_lines) do
    lu.assertEquals(path, expected[i], "mismatch at line " .. i)
  end
end
