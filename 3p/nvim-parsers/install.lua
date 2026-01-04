#!/usr/bin/env lua
-- Installs treesitter parsers
-- Usage: install.lua <nvim_staged> <treesitter_staged> <output_dir> <parsers_config>

local cosmo = require("cosmo")
local path = require("cosmo.path")
local unix = require("cosmo.unix")
local spawn = require("cosmic.spawn")

local function install(nvim_staged, treesitter_staged, output_dir, parsers_config)
  local nvim_bin = path.join(nvim_staged, "bin/nvim")

  io.stderr:write(string.format("nvim-parsers: nvim_bin=%s\n", nvim_bin))
  io.stderr:write(string.format("nvim-parsers: treesitter_staged=%s\n", treesitter_staged))
  io.stderr:write(string.format("nvim-parsers: output_dir=%s\n", output_dir))
  io.stderr:write(string.format("nvim-parsers: parsers_config=%s\n", parsers_config))

  -- check nvim is executable
  local handle = spawn({nvim_bin, "--version"})
  local ok = handle:read()
  if not ok then
    io.stderr:write("nvim-parsers: skipped (nvim not executable on this platform)\n")
    return true
  end
  io.stderr:write("nvim-parsers: nvim is executable\n")

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

  io.stderr:write("nvim-parsers: running nvim to install parsers\n")
  handle = spawn({nvim_bin, "--headless", "-u", "NONE", "-l", script_path}, {env = env})
  -- read stderr before wait() since wait() drains and closes it
  local stderr = handle.stderr:read()
  local exit_code = handle:wait()
  io.stderr:write(string.format("nvim-parsers: nvim exit_code=%s\n", tostring(exit_code)))
  if stderr and stderr ~= "" then
    io.stderr:write("nvim-parsers: nvim stderr:\n  " .. stderr:gsub("\n", "\n  ") .. "\n")
  end
  if exit_code ~= 0 then
    io.stderr:write("nvim-parsers: installation failed\n")
    io.stderr:write(string.format("  script: %s\n", script_path))
    io.stderr:write(string.format("  exit: %d\n", exit_code or -1))
    return nil, "parser installation failed"
  end
  -- check what was created
  local parser_dir = path.join(cwd, output_dir, "parser")
  local st = unix.stat(parser_dir)
  if st then
    io.stderr:write(string.format("nvim-parsers: parser_dir exists: %s\n", parser_dir))
  else
    io.stderr:write(string.format("nvim-parsers: parser_dir MISSING: %s\n", parser_dir))
  end
  return true
end

if cosmo.is_main() then
  local ok = install(arg[1], arg[2], arg[3], arg[4])
  if not ok then os.exit(1) end
end

return { install = install }
