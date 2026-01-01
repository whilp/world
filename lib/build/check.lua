#!/usr/bin/env lua
local lfs = require('lfs')
local luacheck = require('luacheck')

local function is_test_file(name)
  return name:match('^test')
end

local function find_lua_files(dir, files)
  files = files or {}
  for entry in lfs.dir(dir) do
    if entry ~= '.' and entry ~= '..' then
      local path = dir .. '/' .. entry
      local attr = lfs.attributes(path)
      if attr and attr.mode == 'directory' then
        find_lua_files(path, files)
      elseif entry:match('%.lua$') and not is_test_file(entry) then
        table.insert(files, path)
      end
    end
  end
  return files
end

local dirs = {...}
if #dirs == 0 then
  dirs = {'lib', '3p'}
end

local all_files = {}
for _, dir in ipairs(dirs) do
  find_lua_files(dir, all_files)
end

table.sort(all_files)
io.stderr:write('Checking ' .. #all_files .. ' files...\n')

local report = luacheck.check_files(all_files)

local has_issues = false
for i, file_report in ipairs(report) do
  if file_report.error then
    has_issues = true
    print(all_files[i] .. ': ' .. file_report.error)
  end
  for _, event in ipairs(file_report) do
    has_issues = true
    print(all_files[i] .. ':' .. event.line .. ':' .. event.column .. ': ' .. luacheck.get_message(event))
  end
end

if has_issues then
  os.exit(1)
else
  io.stderr:write('No issues found.\n')
end
