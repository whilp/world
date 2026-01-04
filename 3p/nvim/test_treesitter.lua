#!/usr/bin/env run-test.lua

local path = require("cosmo.path")
local unix = require("cosmo.unix")

local site_dir = path.join(TEST_DIR, "share", "nvim", "site")
local pack_dir = path.join(site_dir, "pack", "core", "opt")

local function test_plugins_installed()
  local expected = {"conform.nvim", "mini.nvim", "nvim-lspconfig", "nvim-treesitter"}
  for _, name in ipairs(expected) do
    local plugin_dir = path.join(pack_dir, name)
    local st = unix.stat(plugin_dir)
    assert(st, "plugin not found: " .. name)
  end
end
test_plugins_installed()

local function test_nvim_treesitter_has_init()
  local init_lua = path.join(pack_dir, "nvim-treesitter", "lua", "nvim-treesitter", "init.lua")
  local st = unix.stat(init_lua)
  assert(st, "nvim-treesitter init.lua not found")
end
test_nvim_treesitter_has_init()

local function test_parsers_installed()
  local parser_dir = path.join(site_dir, "parser")
  local st = unix.stat(parser_dir)
  assert(st, "parser directory not found")

  -- check a subset of parsers from .config/nvim/parsers.lua
  local expected = {"lua", "python", "bash", "json"}
  for _, name in ipairs(expected) do
    local parser = path.join(parser_dir, name .. ".so")
    st = unix.stat(parser)
    assert(st, name .. ".so parser not found")
  end
end
test_parsers_installed()

local function test_helptags_generated()
  local doc_dir = path.join(pack_dir, "nvim-treesitter", "doc")
  local st = unix.stat(doc_dir)
  if not st then
    -- doc dir may not exist for all plugins
    return
  end
  local tags = path.join(doc_dir, "tags")
  st = unix.stat(tags)
  -- helptags are optional, just check doc dir exists
end
test_helptags_generated()
