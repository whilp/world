#!/usr/bin/env run-test.lua

local path = require("cosmo.path")
local unix = require("cosmo.unix")

local site_dir = path.join(TEST_DIR, "share", "nvim", "site")

local function test_nvim_treesitter_plugin_exists()
  local plugin_dir = path.join(site_dir, "pack", "core", "opt", "nvim-treesitter")
  local init_lua = path.join(plugin_dir, "lua", "nvim-treesitter", "init.lua")
  local st = unix.stat(init_lua)
  assert(st, "nvim-treesitter plugin not found at " .. init_lua)
end
test_nvim_treesitter_plugin_exists()

local function test_treesitter_parsers_installed()
  local parser_dir = path.join(site_dir, "parser")
  local st = unix.stat(parser_dir)
  assert(st, "parser directory not found at " .. parser_dir)

  -- verify at least one parser is installed (lua is in parsers.lua)
  local lua_parser = path.join(parser_dir, "lua.so")
  st = unix.stat(lua_parser)
  assert(st, "lua.so parser not found at " .. lua_parser)
end
test_treesitter_parsers_installed()
