#!/usr/bin/env lua
-- Installs treesitter parsers
-- Usage: install.lua <nvim_staged> <treesitter_staged> <output_dir> <parsers_config>

local cosmo = require("cosmo")
local path = require("cosmo.path")
local unix = require("cosmo.unix")
local spawn = require("cosmic.spawn")

local function install(nvim_staged, treesitter_staged, output_dir, parsers_config, tree_sitter_staged)
  local nvim_bin = path.join(nvim_staged, "bin/nvim")

  -- check nvim is executable
  local handle = spawn({nvim_bin, "--version"})
  local ok = handle:read()
  if not ok then
    io.stderr:write("nvim-parsers: skipped (nvim not executable on this platform)\n")
    return true
  end

  local cwd = unix.getcwd()
  local parsers = dofile(parsers_config)
  local cache_dir = path.join(cwd, output_dir, "cache")
  unix.makedirs(cache_dir)

  local abs_treesitter = path.join(cwd, treesitter_staged)
  local abs_output = path.join(cwd, output_dir)
  local script = string.format([[
vim.opt.runtimepath:prepend("%s")
local ts = require("nvim-treesitter")
ts.setup({ install_dir = "%s" })
local task = ts.install({"%s"})
local ok, err = task:pwait(300000)
if ok then
  vim.cmd("qall!")
else
  io.stderr:write("pwait failed: " .. tostring(err) .. "\n")
  vim.cmd("cquit 1")
end
]], abs_treesitter, abs_output, table.concat(parsers, '","'))

  local script_path = path.join(cache_dir, "install.lua")
  cosmo.Barf(script_path, script)

  -- unix.environ() returns array of "KEY=VALUE" strings
  -- helper to set or update an env var in the array
  local function set_env(env, key, value)
    local prefix = key .. "="
    for i, entry in ipairs(env) do
      if entry:sub(1, #prefix) == prefix then
        env[i] = prefix .. value
        return
      end
    end
    table.insert(env, prefix .. value)
  end

  local function get_env(env, key)
    local prefix = key .. "="
    for _, entry in ipairs(env) do
      if entry:sub(1, #prefix) == prefix then
        return entry:sub(#prefix + 1)
      end
    end
    return nil
  end

  local env = unix.environ()
  set_env(env, "XDG_CACHE_HOME", cache_dir)
  set_env(env, "VIMRUNTIME", path.join(cwd, nvim_staged, "share/nvim/runtime"))
  set_env(env, "VIM", path.join(cwd, nvim_staged, "share/nvim"))
  if not get_env(env, "CC") then
    set_env(env, "CC", "cc")
  end

  -- add tree-sitter CLI to PATH
  if tree_sitter_staged then
    local ts_bin = path.join(cwd, tree_sitter_staged)
    local current_path = get_env(env, "PATH") or ""
    set_env(env, "PATH", ts_bin .. ":" .. current_path)
  end

  handle = spawn({nvim_bin, "--headless", "-u", "NONE", "-l", script_path}, {env = env})
  -- read stderr before wait() since wait() drains and closes it
  local stderr = handle.stderr:read()
  local exit_code = handle:wait()
  if exit_code ~= 0 then
    io.stderr:write("nvim-parsers: installation failed\n")
    io.stderr:write(string.format("  script: %s\n", script_path))
    io.stderr:write(string.format("  exit: %d\n", exit_code or -1))
    if stderr and stderr ~= "" then
      io.stderr:write("  output:\n    " .. stderr:gsub("\n", "\n    ") .. "\n")
    end
    return nil, "parser installation failed"
  end
  return true
end

if cosmo.is_main() then
  local ok = install(arg[1], arg[2], arg[3], arg[4], arg[5])
  if not ok then os.exit(1) end
end

return { install = install }
