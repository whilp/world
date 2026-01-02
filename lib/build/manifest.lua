#!/usr/bin/env lua
local spawn = require("spawn").spawn
local unix = require("cosmo.unix")

local default_excluded = {
  "^%.config/hammerspoon/",
  "^%.config/nvim/",
  "^%.config/voyager/",
  "^%.local/bin/",
  "^o/",
}

local function is_excluded(file, excluded_patterns)
  for _, pattern in ipairs(excluded_patterns) do
    if file:match(pattern) then
      return true
    end
  end
  return false
end

local function has_lua_shebang(filepath)
  local fd = unix.open(filepath, unix.O_RDONLY)
  if not fd then
    return false
  end

  local first_line = unix.read(fd, 256)
  unix.close(fd)

  if not first_line or first_line == "" then
    return false
  end

  local line = first_line:match("^([^\r\n]+)")
  if not line then
    return false
  end

  return line:match("^#!%s*/.*lua") ~= nil
end

local function find_lua_files(opts)
  opts = opts or {}
  local excluded_patterns = opts.excluded_patterns or default_excluded
  local files = {}

  local handle = spawn({"git", "ls-files", "*.lua"})
  local ok, stdout, exit_code = handle:read()

  if not ok then
    return nil, "git ls-files failed"
  end

  if exit_code == 0 and stdout then
    for line in stdout:gmatch("[^\r\n]+") do
      if not is_excluded(line, excluded_patterns) then
        table.insert(files, line)
      end
    end
  end

  handle = spawn({"git", "ls-files"})
  ok, stdout, exit_code = handle:read()

  if not ok then
    return nil, "git ls-files failed"
  end

  if exit_code == 0 and stdout then
    for line in stdout:gmatch("[^\r\n]+") do
      if not line:match("%.lua$") and not is_excluded(line, excluded_patterns) then
        if has_lua_shebang(line) then
          table.insert(files, line)
        end
      end
    end
  end

  table.sort(files)
  return files
end

local function main(...)
  local args = {...}
  local opts = {}

  if #args > 0 then
    opts.excluded_patterns = {}
    for _, pattern in ipairs(args) do
      table.insert(opts.excluded_patterns, pattern)
    end
  end

  local files, err = find_lua_files(opts)
  if not files then
    io.stderr:write("error: " .. err .. "\n")
    os.exit(1)
  end

  for _, file in ipairs(files) do
    print(file)
  end

  return 0
end

if not pcall(debug.getlocal, 4, 1) then
  os.exit(main(...))
end

return {
  find_lua_files = find_lua_files,
  has_lua_shebang = has_lua_shebang,
}
