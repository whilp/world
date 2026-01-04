#!/usr/bin/env run-test.lua

local spawn = require("cosmic.spawn")
local path = require("cosmo.path")
local unix = require("cosmo.unix")

local bin = path.join(TEST_DIR, "bin", "nvim")
local site_dir = path.join(TEST_DIR, "share", "nvim", "site")

local function test_nvim_treesitter_plugin_exists()
  local plugin_dir = path.join(site_dir, "pack", "core", "opt", "nvim-treesitter")
  local init_lua = path.join(plugin_dir, "lua", "nvim-treesitter", "init.lua")
  local st = unix.stat(init_lua)
  assert(st, "nvim-treesitter plugin not found at " .. init_lua)
end
test_nvim_treesitter_plugin_exists()

local function test_treesitter_can_install_parser()
  -- test that nvim-treesitter can install a parser on demand
  -- this requires tree-sitter CLI which should be in TEST_BIN
  local tree_sitter_bin = path.join(os.getenv("TEST_BIN"), "tree-sitter")
  local st = unix.stat(tree_sitter_bin)
  if not st then
    -- skip test if tree-sitter not available
    io.write("  skipping: tree-sitter CLI not found\n")
    return
  end

  -- create temp directories for test
  local tmp_data = path.join(TEST_TMPDIR, "nvim-data")
  local tmp_cache = path.join(TEST_TMPDIR, "nvim-cache")
  local tmp_config = path.join(TEST_TMPDIR, "nvim-config")
  unix.makedirs(tmp_data)
  unix.makedirs(tmp_cache)
  unix.makedirs(tmp_config)

  -- create a minimal init.lua that loads nvim-treesitter
  local init_lua = path.join(tmp_config, "init.lua")
  local fd = unix.open(init_lua, unix.O_WRONLY | unix.O_CREAT | unix.O_TRUNC, tonumber("644", 8))
  unix.write(fd, [[
vim.opt.rtp:prepend(']] .. site_dir .. [[')
vim.cmd('packadd nvim-treesitter')
require('nvim-treesitter').setup({ install_dir = vim.fn.stdpath('data') })
]])
  unix.close(fd)

  -- run nvim and install the c parser (simpler than lua, fewer deps)
  local lua_cmd = [[
    local ts = require('nvim-treesitter')
    local ok = ts.install({'c'}):wait()
    if ok then
      vim.cmd('qall!')
    else
      vim.cmd('cquit 1')
    end
  ]]

  local env_path = os.getenv("TEST_BIN") .. ":" .. os.getenv("PATH")
  local ok, stdout, stderr = spawn({
    "env",
    "PATH=" .. env_path,
    "XDG_DATA_HOME=" .. tmp_data,
    "XDG_CACHE_HOME=" .. tmp_cache,
    "XDG_CONFIG_HOME=" .. tmp_config,
    bin, "--headless", "-u", init_lua,
    "+lua " .. lua_cmd:gsub("\n", " "),
  }, { timeout = 60000 }):read()

  if not ok then
    error("parser installation failed: " .. (stderr or stdout or "unknown error"))
  end

  -- verify the parser was installed
  local parser_so = path.join(tmp_data, "nvim", "site", "parser", "c.so")
  local parser_st = unix.stat(parser_so)
  assert(parser_st, "c.so parser not found after install at " .. parser_so)
end
test_treesitter_can_install_parser()
