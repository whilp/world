#!/usr/bin/env run-test.lua

local spawn = require("cosmic.spawn")
local path = require("cosmo.path")
local unix = require("cosmo.unix")

local bin = path.join(TEST_DIR, "bin", "nvim")
local config_home = path.join(TEST_DIR, "share", "nvim", "site", "pack", "core", "opt")

local function test_lua_parser_installed()
  -- check that lua parser exists in the staged nvim
  local parser_dir = path.join(TEST_DIR, "share", "nvim", "site", "parser")
  local lua_parser = path.join(parser_dir, "lua.so")
  local st = unix.stat(lua_parser)
  assert(st, "lua.so parser not found at " .. lua_parser)
end
test_lua_parser_installed()

local function test_treesitter_start()
  -- create a temp lua file
  local tmp_lua = path.join(TEST_TMPDIR, "test.lua")
  local fd = unix.open(tmp_lua, unix.O_WRONLY | unix.O_CREAT | unix.O_TRUNC, tonumber("644", 8))
  unix.write(fd, "local x = 1\n")
  unix.close(fd)

  -- run nvim and try to start treesitter on a lua buffer
  local lua_cmd = [[
    vim.opt.rtp:prepend(']] .. config_home .. [[/nvim-treesitter')
    local ok, err = pcall(function()
      vim.treesitter.start(0, 'lua')
    end)
    if not ok then
      io.stderr:write('treesitter error: ' .. tostring(err) .. '\n')
      vim.cmd('cquit 1')
    else
      vim.cmd('qall!')
    end
  ]]

  local ok, stdout, stderr = spawn({
    bin, "--headless", "-u", "NONE",
    "--cmd", "set rtp+=" .. path.join(TEST_DIR, "share", "nvim", "site"),
    tmp_lua,
    "+lua " .. lua_cmd:gsub("\n", " "),
  }):read()

  if not ok then
    error("treesitter.start() failed: " .. (stderr or stdout or "unknown error"))
  end
end
test_treesitter_start()
