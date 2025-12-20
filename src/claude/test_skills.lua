package.path = os.getenv("HOME") .. "/.local/lib/lua/?.lua;" .. package.path

local cosmo = require('cosmo')
local unix = cosmo.unix

local script_dir = cosmo.path.dirname(debug.getinfo(1, "S").source:sub(2))
local dotfiles_root = script_dir .. "/../.."
local skills_dir = dotfiles_root .. "/.claude/skills"


function test_lua_skill_file_exists()
  local skill_path = skills_dir .. "/lua/SKILL.md"
  lu.assertTrue(unix.stat(skill_path) ~= nil, "lua/SKILL.md should exist")
end

function test_lua_skill_has_valid_frontmatter()
  local skill_path = skills_dir .. "/lua/SKILL.md"
  local f = io.open(skill_path, "r")
  lu.assertTrue(f ~= nil, "should be able to open SKILL.md")

  local content = f:read("*all")
  f:close()

  lu.assertTrue(content:match("^---\n") ~= nil, "should start with --- delimiter")
  lu.assertTrue(content:match("\nname: lua\n") ~= nil, "should have name field")
  lu.assertTrue(content:match("\ndescription:") ~= nil, "should have description field")
  lu.assertTrue(content:match("\nallowed%-tools:") ~= nil, "should have allowed-tools field")
end

function test_lua_skill_templates_directory_exists()
  local templates_dir = skills_dir .. "/lua/templates"
  lu.assertTrue(unix.stat(templates_dir) ~= nil, "templates directory should exist")
end

function test_lua_skill_executable_template_exists()
  local template_path = skills_dir .. "/lua/templates/executable.lua"
  lu.assertTrue(unix.stat(template_path) ~= nil, "executable.lua template should exist")
end

function test_lua_skill_executable_template_has_valid_syntax()
  local template_path = skills_dir .. "/lua/templates/executable.lua"
  local func, err = loadfile(template_path)

  lu.assertTrue(func ~= nil, "executable.lua should have valid syntax: " .. tostring(err))
end

function test_lua_skill_main_template_exists()
  local template_path = skills_dir .. "/lua/templates/main.lua"
  lu.assertTrue(unix.stat(template_path) ~= nil, "main.lua template should exist")
end

function test_lua_skill_main_template_has_valid_syntax()
  local template_path = skills_dir .. "/lua/templates/main.lua"
  local func, err = loadfile(template_path)

  lu.assertTrue(func ~= nil, "main.lua should have valid syntax: " .. tostring(err))
end

function test_lua_skill_module_template_exists()
  local template_path = skills_dir .. "/lua/templates/module.lua"
  lu.assertTrue(unix.stat(template_path) ~= nil, "module.lua template should exist")
end

function test_lua_skill_module_template_has_valid_syntax()
  local template_path = skills_dir .. "/lua/templates/module.lua"
  local func, err = loadfile(template_path)

  lu.assertTrue(func ~= nil, "module.lua should have valid syntax: " .. tostring(err))
end

function test_lua_skill_test_template_exists()
  local template_path = skills_dir .. "/lua/templates/test.lua"
  lu.assertTrue(unix.stat(template_path) ~= nil, "test.lua template should exist")
end

function test_lua_skill_test_template_has_valid_syntax()
  local template_path = skills_dir .. "/lua/templates/test.lua"
  local func, err = loadfile(template_path)

  lu.assertTrue(func ~= nil, "test.lua should have valid syntax: " .. tostring(err))
end

function test_lua_skill_executable_template_is_executable_script()
  local template_path = skills_dir .. "/lua/templates/executable.lua"
  local f = io.open(template_path, "r")
  lu.assertTrue(f ~= nil, "should be able to open executable.lua")

  local content = f:read("*all")
  f:close()

  lu.assertTrue(content:match("^#!") ~= nil, "executable.lua should have shebang")
end

function test_lua_skill_templates_are_readable()
  local templates = {"executable.lua", "main.lua", "module.lua", "test.lua"}

  for _, template_name in ipairs(templates) do
    local template_path = skills_dir .. "/lua/templates/" .. template_name
    local f = io.open(template_path, "r")
    lu.assertTrue(f ~= nil, template_name .. " should be readable")

    local content = f:read("*all")
    f:close()

    lu.assertTrue(#content > 0, template_name .. " should not be empty")
  end
end

function test_lua_skill_documentation_has_content()
  local skill_path = skills_dir .. "/lua/SKILL.md"
  local f = io.open(skill_path, "r")
  lu.assertTrue(f ~= nil, "should be able to open SKILL.md")

  local content = f:read("*all")
  f:close()

  lu.assertTrue(#content > 1000, "SKILL.md should have substantial content")
  lu.assertTrue(content:match("# ") ~= nil, "SKILL.md should have markdown headers")
end
