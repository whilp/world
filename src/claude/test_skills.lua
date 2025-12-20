package.path = os.getenv("HOME") .. "/.local/lib/lua/?.lua;" .. package.path

local cosmo = require('cosmo')
local unix = cosmo.unix

local script_dir = cosmo.path.dirname(debug.getinfo(1, "S").source:sub(2))
local dotfiles_root = script_dir .. "/../.."
local skills_dir = dotfiles_root .. "/.claude/skills"

local function file_exists(path)
  return unix.stat(path) ~= nil
end

local function read_file(path)
  local f = io.open(path, "r")
  if not f then
    return nil
  end
  local content = f:read("*all")
  f:close()
  return content
end

local function extract_frontmatter(content)
  local lines = {}
  local in_frontmatter = false
  local frontmatter_lines = {}

  for line in content:gmatch("[^\n]+") do
    table.insert(lines, line)
  end

  if lines[1] ~= "---" then
    return nil
  end

  for i = 2, #lines do
    if lines[i] == "---" then
      return table.concat(frontmatter_lines, "\n")
    end
    table.insert(frontmatter_lines, lines[i])
  end

  return nil
end

local function parse_yaml_simple(yaml)
  local result = {}
  for line in yaml:gmatch("[^\n]+") do
    local key, value = line:match("^([%w_-]+):%s*(.+)$")
    if key and value then
      -- Handle array notation [item1, item2]
      if value:match("^%[.+%]$") then
        local items = {}
        for item in value:gmatch("[%w_-]+") do
          table.insert(items, item)
        end
        result[key] = items
      else
        result[key] = value
      end
    end
  end
  return result
end

function test_lua_skill_file_exists()
  local skill_path = skills_dir .. "/lua/SKILL.md"
  lu.assertTrue(file_exists(skill_path), "lua/SKILL.md should exist")
end

function test_lua_skill_has_valid_frontmatter()
  local skill_path = skills_dir .. "/lua/SKILL.md"
  local content = read_file(skill_path)

  lu.assertTrue(content ~= nil, "should be able to read SKILL.md")
  lu.assertTrue(content:match("^---\n") ~= nil, "should start with --- delimiter")

  local frontmatter = extract_frontmatter(content)
  lu.assertTrue(frontmatter ~= nil, "should have valid frontmatter")
end

function test_lua_skill_frontmatter_has_required_fields()
  local skill_path = skills_dir .. "/lua/SKILL.md"
  local content = read_file(skill_path)
  local frontmatter = extract_frontmatter(content)
  local fields = parse_yaml_simple(frontmatter)

  lu.assertTrue(fields.name ~= nil, "frontmatter should have 'name' field")
  lu.assertEquals(fields.name, "lua", "name should be 'lua'")
  lu.assertTrue(fields.description ~= nil, "frontmatter should have 'description' field")
  lu.assertTrue(type(fields["allowed-tools"]) == "table", "should have allowed-tools array")
end

function test_lua_skill_templates_directory_exists()
  local templates_dir = skills_dir .. "/lua/templates"
  lu.assertTrue(file_exists(templates_dir), "templates directory should exist")
end

function test_lua_skill_executable_template_exists()
  local template_path = skills_dir .. "/lua/templates/executable.lua"
  lu.assertTrue(file_exists(template_path), "executable.lua template should exist")
end

function test_lua_skill_executable_template_has_valid_syntax()
  local template_path = skills_dir .. "/lua/templates/executable.lua"
  local func, err = loadfile(template_path)

  lu.assertTrue(func ~= nil, "executable.lua should have valid syntax: " .. tostring(err))
end

function test_lua_skill_main_template_exists()
  local template_path = skills_dir .. "/lua/templates/main.lua"
  lu.assertTrue(file_exists(template_path), "main.lua template should exist")
end

function test_lua_skill_main_template_has_valid_syntax()
  local template_path = skills_dir .. "/lua/templates/main.lua"
  local func, err = loadfile(template_path)

  lu.assertTrue(func ~= nil, "main.lua should have valid syntax: " .. tostring(err))
end

function test_lua_skill_module_template_exists()
  local template_path = skills_dir .. "/lua/templates/module.lua"
  lu.assertTrue(file_exists(template_path), "module.lua template should exist")
end

function test_lua_skill_module_template_has_valid_syntax()
  local template_path = skills_dir .. "/lua/templates/module.lua"
  local func, err = loadfile(template_path)

  lu.assertTrue(func ~= nil, "module.lua should have valid syntax: " .. tostring(err))
end

function test_lua_skill_test_template_exists()
  local template_path = skills_dir .. "/lua/templates/test.lua"
  lu.assertTrue(file_exists(template_path), "test.lua template should exist")
end

function test_lua_skill_test_template_has_valid_syntax()
  local template_path = skills_dir .. "/lua/templates/test.lua"
  local func, err = loadfile(template_path)

  lu.assertTrue(func ~= nil, "test.lua should have valid syntax: " .. tostring(err))
end

function test_lua_skill_executable_template_is_executable_script()
  local template_path = skills_dir .. "/lua/templates/executable.lua"
  local content = read_file(template_path)

  lu.assertTrue(content:match("^#!") ~= nil, "executable.lua should have shebang")
end

function test_lua_skill_templates_are_readable()
  local templates = {"executable.lua", "main.lua", "module.lua", "test.lua"}

  for _, template_name in ipairs(templates) do
    local template_path = skills_dir .. "/lua/templates/" .. template_name
    local content = read_file(template_path)

    lu.assertTrue(content ~= nil, template_name .. " should be readable")
    lu.assertTrue(#content > 0, template_name .. " should not be empty")
  end
end

function test_lua_skill_documentation_has_content()
  local skill_path = skills_dir .. "/lua/SKILL.md"
  local content = read_file(skill_path)

  lu.assertTrue(#content > 1000, "SKILL.md should have substantial content")
  lu.assertTrue(content:match("# ") ~= nil, "SKILL.md should have markdown headers")
end
