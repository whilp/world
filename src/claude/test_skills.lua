package.path = os.getenv("HOME") .. "/.local/lib/lua/?.lua;" .. package.path

local cosmo = require('cosmo')
local unix = cosmo.unix
local path = cosmo.path

local script_dir = cosmo.path.dirname(debug.getinfo(1, "S").source:sub(2))
local dotfiles_root = path.join(script_dir, "../..")
local skills_dir = path.join(dotfiles_root, ".claude/skills")


function test_lua_skill_file_exists()
  local skill_path = path.join(skills_dir, "lua/SKILL.md")
  lu.assertTrue(unix.stat(skill_path) ~= nil, "lua/SKILL.md should exist")
end

function test_lua_skill_has_valid_frontmatter()
  local skill_path = path.join(skills_dir, "lua/SKILL.md")
  local f = io.open(skill_path, "r")
  lu.assertTrue(f ~= nil, "should be able to open SKILL.md")

  local content = f:read("*all")
  f:close()

  lu.assertTrue(content:match("^---\n") ~= nil, "should start with --- delimiter")
  lu.assertTrue(content:match("\nname: lua\n") ~= nil, "should have name field")
  lu.assertTrue(content:match("\ndescription:") ~= nil, "should have description field")
  lu.assertTrue(content:match("\nallowed%-tools:") ~= nil, "should have allowed-tools field")
end
