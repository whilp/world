-- luacheck ignore: neovim runtime
-- Teal loader: compiles .tl files on require() using cosmic-lua
local M = {}

local home = vim.fn.expand("~")
local cosmic = home .. "/.local/bin/cosmic-lua"
local tl_gen = "/zip/tl-gen.lua"

-- Search paths for .tl files (mirrors package.path but for .tl)
local teal_paths = {
  home .. "/lib/?.tl",
  home .. "/lib/?/init.tl",
}

local function find_tl_file(name)
  local path_name = name:gsub("%.", "/")
  for _, pattern in ipairs(teal_paths) do
    local path = pattern:gsub("%?", path_name)
    if vim.fn.filereadable(path) == 1 then
      return path
    end
  end
  return nil
end

local function compile_tl(tl_path)
  -- Compile to temp file
  local tmp = vim.fn.tempname() .. ".lua"
  local cmd = string.format("%s %s -- %s -o %s 2>&1", cosmic, tl_gen, tl_path, tmp)
  local output = vim.fn.system(cmd)
  local exit_code = vim.v.shell_error

  if exit_code ~= 0 then
    vim.fn.delete(tmp)
    return nil, "teal compile failed: " .. output
  end

  -- Read compiled lua
  local f = io.open(tmp, "r")
  if not f then
    return nil, "failed to read compiled output"
  end
  local lua_code = f:read("*a")
  f:close()
  vim.fn.delete(tmp)

  return lua_code, nil
end

-- Package loader for .tl files
local function teal_loader(name)
  local tl_path = find_tl_file(name)
  if not tl_path then
    return nil
  end

  local lua_code, err = compile_tl(tl_path)
  if not lua_code then
    error(err)
  end

  local chunk, load_err = loadstring(lua_code, "@" .. tl_path)
  if not chunk then
    error("failed to load compiled teal: " .. load_err)
  end

  return chunk
end

function M.setup()
  -- Check cosmic exists
  if vim.fn.executable(cosmic) ~= 1 then
    return false
  end

  -- Add teal loader after lua loaders (position 3)
  table.insert(package.loaders, 3, teal_loader)
  return true
end

return M
