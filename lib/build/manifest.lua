#!/usr/bin/env lua
local spawn = require("cosmic.spawn")
local unix = require("cosmo.unix")

local default_excluded = {
  "^%.config/hammerspoon/",
  "^%.config/nvim/",
  "^%.config/voyager/",
  "^%.local/bin/",
  "^o/",
}

local function is_excluded(path, patterns)
  for _, pattern in ipairs(patterns) do
    if path:match(pattern) then
      return true
    end
  end
  return false
end

local function read_first_line(filepath)
  local fd = unix.open(filepath, unix.O_RDONLY)
  if not fd then
    return nil
  end
  local chunk = unix.read(fd, 256)
  unix.close(fd)
  if not chunk then
    return nil
  end
  return chunk:match("^([^\r\n]+)")
end

local shebang_patterns = {
  {"lua", "lua"},
  {"bash", "shell"},
  {"sh", "shell"},
  {"python", "python"},
}

local function detect_shebang_type(first_line)
  if not first_line then
    return nil
  end
  for _, entry in ipairs(shebang_patterns) do
    if first_line:match("^#!%s*/.*" .. entry[1]) then
      return entry[2]
    end
  end
  return nil
end

local ext_types = {
  lua = "lua",
  sh = "shell",
  bash = "shell",
  py = "python",
  mk = "make",
  md = "markdown",
  yml = "yaml",
  yaml = "yaml",
  json = "json",
  toml = "toml",
}

local function detect_type(path)
  local ext = path:match("%.([^.]+)$")
  local file_type = ext_types[ext]
  if file_type then
    return file_type
  end
  local first_line = read_first_line(path)
  return detect_shebang_type(first_line)
end

local function is_test_file(path)
  local basename = path:match("([^/]+)$")
  if not basename then
    return false
  end
  if basename:match("^test_") then
    return true
  end
  if basename:match("test%.lua$") then
    return true
  end
  return false
end

local function git_files_iter(stdout)
  local pos = 1
  local len = #stdout
  return function()
    if pos > len then
      return nil
    end
    local null_pos = stdout:find("\0", pos, true)
    if not null_pos then
      local path = stdout:sub(pos)
      pos = len + 1
      if path ~= "" then
        return path
      end
      return nil
    end
    local path = stdout:sub(pos, null_pos - 1)
    pos = null_pos + 1
    return path
  end
end

local function files(opts)
  opts = opts or {}
  local detect_type_fn = opts._detect_type or detect_type

  local stdout = opts._git_output
  if not stdout then
    local handle = spawn({"git", "ls-files", "-z"})
    local ok, out, exit_code = handle:read()
    if not ok or exit_code ~= 0 or not out then
      return function() return nil end
    end
    stdout = out
  end

  local iter = git_files_iter(stdout)

  return function()
    local path = iter()
    if not path then
      return nil
    end
    return {
      path = path,
      type = detect_type_fn(path),
      is_test = is_test_file(path),
    }
  end
end

local function collect(iter)
  local results = {}
  for item in iter do
    table.insert(results, item)
  end
  table.sort(results, function(a, b) return a.path < b.path end)
  return results
end

local function find_lua_files(opts)
  opts = opts or {}
  local excluded = opts.excluded_patterns or default_excluded
  local paths = {}
  for f in files(opts) do
    if f.type == "lua" and not is_excluded(f.path, excluded) then
      table.insert(paths, f.path)
    end
  end
  table.sort(paths)
  return paths
end

local function find_lua_tests(opts)
  opts = opts or {}
  local excluded = opts.excluded_patterns or default_excluded
  local paths = {}
  for f in files(opts) do
    if f.type == "lua" and f.is_test and not is_excluded(f.path, excluded) then
      table.insert(paths, f.path)
    end
  end
  table.sort(paths)
  return paths
end

local filters = {
  find_lua_files = find_lua_files,
  find_lua_tests = find_lua_tests,
}

local function main(...)
  local args = {...}
  local filter_name = args[1] or "find_lua_files"

  local filter = filters[filter_name]
  if not filter then
    io.stderr:write("unknown filter: " .. filter_name .. "\n")
    io.stderr:write("available: find_lua_files, find_lua_tests\n")
    return 1
  end

  for _, path in ipairs(filter()) do
    print(path)
  end

  return 0
end

if not pcall(debug.getlocal, 4, 1) then
  os.exit(main(...))
end

return {
  files = files,
  collect = collect,
  find_lua_files = find_lua_files,
  find_lua_tests = find_lua_tests,
  detect_type = detect_type,
  is_test_file = is_test_file,
  is_excluded = is_excluded,
  default_excluded = default_excluded,
  git_files_iter = git_files_iter,
}
