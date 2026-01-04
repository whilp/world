#!/usr/bin/env lua
-- Installs treesitter parsers
-- Usage: install.lua <nvim_staged> <treesitter_staged> <output_dir>

local cosmo = require("cosmo")
local path = require("cosmo.path")
local unix = require("cosmo.unix")
local spawn = require("cosmic.spawn")

local function install(nvim_staged, treesitter_staged, output_dir)
  local nvim_bin = path.join(nvim_staged, "bin/nvim")

  -- check nvim is executable
  local handle = spawn({nvim_bin, "--version"})
  local ok = handle:read()
  if not ok then
    io.write("nvim-parsers: skipped (nvim not executable)\n")
    return true
  end

  local cwd = unix.getcwd()
  local parsers = dofile(path.join(cwd, ".config/nvim/parsers.lua"))
  local cache_dir = path.join(cwd, output_dir, "cache")
  unix.makedirs(cache_dir)

  local script = string.format([[
vim.opt.packpath:prepend("%s")
vim.cmd("packadd nvim-treesitter")
local ts = require("nvim-treesitter")
ts.setup({ install_dir = "%s" })
local ok = ts.install({"%s"}):wait()
if ok then vim.cmd("qall!") else vim.cmd("cquit 1") end
]], treesitter_staged, path.join(cwd, output_dir), table.concat(parsers, '","'))

  local script_path = path.join(cache_dir, "install.lua")
  cosmo.Barf(script_path, script)

  io.write(string.format("nvim-parsers: installing %s\n", table.concat(parsers, ", ")))

  local env = unix.environ()
  env.XDG_CACHE_HOME = cache_dir
  env.VIMRUNTIME = path.join(cwd, nvim_staged, "share/nvim/runtime")
  env.VIM = path.join(cwd, nvim_staged, "share/nvim")

  handle = spawn({nvim_bin, "--headless", "-u", "NONE", "-l", script_path}, {env = env})
  ok = handle:read()
  if not ok then
    local stderr = handle.stderr:read()
    if stderr and stderr ~= "" then
      io.write("  " .. stderr:gsub("\n", "\n  ") .. "\n")
    end
    return nil, "parser installation failed"
  end
  return true
end

if not pcall(debug.getlocal, 4, 1) then
  local ok, err = install(arg[1], arg[2], arg[3])
  if not ok then
    io.stderr:write("error: " .. tostring(err) .. "\n")
    os.exit(1)
  end
end

return { install = install }
