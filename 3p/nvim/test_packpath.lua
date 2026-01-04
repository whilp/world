#!/usr/bin/env run-test.lua
-- teal ignore: test file

local spawn = require("cosmic.spawn")
local path = require("cosmo.path")
local unix = require("cosmo.unix")

local bin = path.join(TEST_DIR, "bin", "nvim")
local bundled_site = path.join(TEST_DIR, "share", "nvim", "site")

-- test: bundled plugins exist
local function test_bundled_plugins_exist()
  local plugins = { "mini.nvim", "nvim-lspconfig", "conform.nvim", "nvim-treesitter" }
  for _, plugin in ipairs(plugins) do
    local plugin_path = path.join(bundled_site, "pack", "core", "opt", plugin)
    assert(unix.stat(plugin_path), "bundled " .. plugin .. " should exist at: " .. plugin_path)
  end
end
test_bundled_plugins_exist()

-- test: bundled plugins load via packadd
local function test_bundled_plugins_load()
  local test_init = path.join(TEST_TMPDIR, "init.lua")
  local f = io.open(test_init, "w")
  f:write([[
local version_dir = vim.fn.fnamemodify(vim.v.progpath, ":h:h")
local bundled_site = version_dir .. "/share/nvim/site"
vim.opt.packpath:prepend(bundled_site)

-- load bundled plugins
vim.cmd.packadd('mini.nvim')
vim.cmd.packadd('nvim-lspconfig')
vim.cmd.packadd('conform.nvim')
vim.cmd.packadd('nvim-treesitter')

-- verify plugins loaded
local results = {}
results.mini = pcall(require, 'mini.bufremove')
results.lspconfig = pcall(require, 'lspconfig')
results.conform = pcall(require, 'conform')
results.treesitter = pcall(require, 'nvim-treesitter')

local result_file = io.open(vim.env.TEST_RESULT_FILE, "w")
for name, ok in pairs(results) do
  result_file:write(name .. "=" .. tostring(ok) .. "\n")
end
result_file:close()
]])
  f:close()

  local result_file = path.join(TEST_TMPDIR, "result.txt")
  local env = unix.environ()
  table.insert(env, "TEST_RESULT_FILE=" .. result_file)
  local ok = spawn({ bin, "--headless", "-u", test_init, "+qa" }, { env = env }):wait()
  assert(ok, "nvim failed to run")

  local result = io.open(result_file, "r")
  assert(result, "result file not created")
  local content = result:read("*a")
  result:close()

  local expected = { "mini", "lspconfig", "conform", "treesitter" }
  for _, name in ipairs(expected) do
    assert(content:find(name .. "=true"), name .. " plugin should load. Result:\n" .. content)
  end
end
test_bundled_plugins_load()
