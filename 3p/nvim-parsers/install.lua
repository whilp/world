#!/usr/bin/env lua
-- Installs treesitter parsers
-- Usage: install.lua <nvim_staged> <treesitter_staged> <output_dir> <parsers_config>

local cosmo = require("cosmo")
local path = require("cosmo.path")
local unix = require("cosmo.unix")
local spawn = require("cosmic.spawn")

local function install(nvim_staged, treesitter_staged, output_dir, parsers_config)
  local nvim_bin = path.join(nvim_staged, "bin/nvim")

  -- check nvim is executable
  local handle = spawn({nvim_bin, "--version"})
  local ok = handle:read()
  if not ok then
    return true -- skip silently if nvim not executable on this platform
  end

  local cwd = unix.getcwd()
  local parsers = dofile(parsers_config)
  local cache_dir = path.join(cwd, output_dir, "cache")
  unix.makedirs(cache_dir)

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
]], treesitter_staged, path.join(cwd, output_dir), table.concat(parsers, '","'))

  local script_path = path.join(cache_dir, "install.lua")
  cosmo.Barf(script_path, script)

  local env = unix.environ()
  env.XDG_CACHE_HOME = cache_dir
  env.VIMRUNTIME = path.join(cwd, nvim_staged, "share/nvim/runtime")
  env.VIM = path.join(cwd, nvim_staged, "share/nvim")
  env.CC = env.CC or "cc"

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
  local ok = install(arg[1], arg[2], arg[3], arg[4])
  if not ok then os.exit(1) end
end

return { install = install }
