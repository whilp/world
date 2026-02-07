#!/usr/bin/env lua
--check:false
-- Installs treesitter parsers
-- Usage: install.lua <nvim_staged> <treesitter_staged> <output_dir> <parsers_config>

local cosmo = require("cosmo")
local path = require("cosmo.path")
local unix = require("cosmo.unix")
local spawn = require("cosmic.child").spawn

local record InstallModule
  install: function(nvim_staged: string, treesitter_staged: string, output_dir: string, parsers_config: string, tree_sitter_staged: string, target: string): boolean, string
end

local function install(nvim_staged: string, treesitter_staged: string, output_dir: string, parsers_config: string, tree_sitter_staged: string, target: string): boolean, string
  local nvim_bin = path.join(nvim_staged, "bin/nvim")

  -- Collect verbose output, only print on error
  local log: {string} = {}
  local function log_write(msg: string)
    table.insert(log, msg)
  end

  log_write(string.format("nvim-parsers: nvim_bin=%s\n", nvim_bin))
  log_write(string.format("nvim-parsers: treesitter_staged=%s\n", treesitter_staged))
  log_write(string.format("nvim-parsers: output_dir=%s\n", output_dir))
  log_write(string.format("nvim-parsers: parsers_config=%s\n", parsers_config))
  log_write(string.format("nvim-parsers: tree_sitter_staged=%s\n", tree_sitter_staged))

  -- check nvim is executable
  local handle = spawn({nvim_bin, "--version"})
  local ok = handle:read()
  if not ok then
    io.stderr:write("nvim-parsers: skipped (nvim not executable on this platform)\n")
    return true
  end
  log_write("nvim-parsers: nvim is executable\n")

  local cwd = unix.getcwd()
  local parsers = dofile(parsers_config) as {string}
  local cache_dir = path.join(cwd, output_dir, "cache")
  unix.makedirs(cache_dir)

  local abs_treesitter = path.join(cwd, treesitter_staged)
  local abs_output = path.join(cwd, output_dir)
  local script = string.format([[
io.stderr:write("nvim-parsers-inner: starting\n")
io.stderr:write("nvim-parsers-inner: PATH=" .. (vim.env.PATH or "nil"):sub(1, 200) .. "\n")
vim.opt.runtimepath:prepend("%s")
io.stderr:write("nvim-parsers-inner: rtp set\n")
local ts = require("nvim-treesitter")
io.stderr:write("nvim-parsers-inner: ts loaded\n")
ts.setup({ install_dir = "%s" })
io.stderr:write("nvim-parsers-inner: ts setup done\n")
local task = ts.install({"%s"})
io.stderr:write("nvim-parsers-inner: install called, waiting...\n")
local ok, err = task:pwait(300000)
io.stderr:write("nvim-parsers-inner: pwait returned ok=" .. tostring(ok) .. " err=" .. tostring(err) .. "\n")
if ok then
  vim.cmd("qall!")
else
  io.stderr:write("pwait failed: " .. tostring(err) .. "\n")
  vim.cmd("cquit 1")
end
]], abs_treesitter, abs_output, table.concat(parsers, '","'))
  log_write(string.format("nvim-parsers: abs_treesitter=%s\n", abs_treesitter))
  log_write(string.format("nvim-parsers: abs_output=%s\n", abs_output))

  local script_path = path.join(cache_dir, "install.lua")
  cosmo.Barf(script_path, script)

  -- unix.environ() returns array of "KEY=VALUE" strings
  -- helper to set or update an env var in the array
  local function set_env(env: {string}, key: string, value: string)
    local prefix = key .. "="
    for i, entry in ipairs(env) do
      if entry:sub(1, #prefix) == prefix then
        env[i] = prefix .. value
        return
      end
    end
    table.insert(env, prefix .. value)
  end

  local function get_env(env: {string}, key: string): string
    local prefix = key .. "="
    for _, entry in ipairs(env) do
      if entry:sub(1, #prefix) == prefix then
        return entry:sub(#prefix + 1)
      end
    end
    return nil
  end

  local env = unix.environ() as {string}
  set_env(env, "XDG_CACHE_HOME", cache_dir)
  set_env(env, "VIMRUNTIME", path.join(cwd, nvim_staged, "share/nvim/runtime"))
  set_env(env, "VIM", path.join(cwd, nvim_staged, "share/nvim"))
  if not get_env(env, "CC") then
    set_env(env, "CC", "cc")
  end

  -- add tree-sitter CLI to PATH
  if tree_sitter_staged then
    local ts_bin = path.join(cwd, tree_sitter_staged, "bin")
    local current_path = get_env(env, "PATH") or ""
    set_env(env, "PATH", ts_bin .. ":" .. current_path)
    log_write(string.format("nvim-parsers: tree-sitter added to PATH: %s\n", ts_bin))
  else
    log_write("nvim-parsers: WARNING: tree_sitter_staged not provided\n")
  end

  log_write("nvim-parsers: running nvim to install parsers\n")
  handle = spawn({nvim_bin, "--headless", "-u", "NONE", "-l", script_path}, {env = env})
  -- read stderr before wait() since wait() drains and closes it
  local stderr = handle.stderr:read()
  local exit_code = handle:wait()
  log_write(string.format("nvim-parsers: nvim exit_code=%s\n", tostring(exit_code)))
  if stderr and stderr ~= "" then
    log_write("nvim-parsers: nvim stderr:\n  " .. (stderr as string):gsub("\n", "\n  ") .. "\n")
  end
  if exit_code ~= 0 then
    -- dump log on error
    io.stderr:write(table.concat(log))
    io.stderr:write("nvim-parsers: installation failed\n")
    io.stderr:write(string.format("  script: %s\n", script_path))
    io.stderr:write(string.format("  exit: %d\n", (exit_code or -1) as integer))
    return nil, "parser installation failed"
  end
  -- check what was created
  local parser_dir = path.join(cwd, output_dir, "parser")
  local st = unix.stat(parser_dir)
  if st then
    log_write(string.format("nvim-parsers: parser_dir exists: %s\n", parser_dir))
  else
    log_write(string.format("nvim-parsers: parser_dir MISSING: %s\n", parser_dir))
  end

  -- Print success message
  local parser_count = #parsers
  if target then
    io.stderr:write(string.format("✓ BUILD  %s (nvim-parsers: %d parsers built)\n", target, parser_count))
  end
  return true
end

if cosmo.is_main() then
  local install_ok = install(arg[1], arg[2], arg[3], arg[4], arg[5], arg[6])
  if not install_ok then os.exit(1) end
end

return { install = install } as InstallModule
