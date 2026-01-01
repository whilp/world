local lu = require("luaunit")
local spawn = require("spawn").spawn
local path = require("cosmo.path")
local unix = require("cosmo.unix")

local bin_dir = os.getenv("TEST_BIN_DIR")
local bin = path.join(bin_dir, "bin", "nvim")

TestNvim = {}

function TestNvim:test_version()
  local handle = spawn({ bin, "--version" })
  lu.assertEquals(handle:wait(), 0)
end

function TestNvim:test_plugin_conform()
  local handle = spawn({ bin, "--headless", "+packadd conform.nvim", "+qa" })
  lu.assertEquals(handle:wait(), 0)
end

function TestNvim:test_plugin_mini()
  local handle = spawn({ bin, "--headless", "+packadd mini.nvim", "+qa" })
  lu.assertEquals(handle:wait(), 0)
end

function TestNvim:test_plugin_lspconfig()
  local handle = spawn({ bin, "--headless", "+packadd nvim-lspconfig", "+qa" })
  lu.assertEquals(handle:wait(), 0)
end

function TestNvim:test_plugin_treesitter()
  local handle = spawn({ bin, "--headless", "+packadd nvim-treesitter", "+qa" })
  lu.assertEquals(handle:wait(), 0)
end

function TestNvim:test_parser_lua()
  local handle = spawn({
    "env",
    "XDG_DATA_HOME=" .. path.join(bin_dir, "share"),
    bin, "--headless",
    "+lua print(vim.treesitter.language.inspect('lua'))",
    "+qa"
  })
  lu.assertEquals(handle:wait(), 0)
end

function TestNvim:test_vimruntime_exists()
  local runtime_dir = path.join(bin_dir, "share", "nvim", "runtime")
  local st = unix.stat(runtime_dir)
  lu.assertNotNil(st, "share/nvim/runtime directory should exist")
end

function TestNvim:test_vimruntime_has_syntax()
  local syntax_file = path.join(bin_dir, "share", "nvim", "runtime", "syntax", "syntax.vim")
  local st = unix.stat(syntax_file)
  lu.assertNotNil(st, "runtime/syntax/syntax.vim should exist")
end

function TestNvim:test_vim_health_loads()
  local env = "VIMRUNTIME=" .. path.join(bin_dir, "share", "nvim", "runtime")
  local handle = spawn({
    "env", env,
    bin, "--headless",
    "+lua assert(vim.health ~= nil, 'vim.health should load')",
    "+qa"
  })
  lu.assertEquals(handle:wait(), 0, "vim.health module should load")
end

function TestNvim:test_vim_lsp_loads()
  local env = "VIMRUNTIME=" .. path.join(bin_dir, "share", "nvim", "runtime")
  local handle = spawn({
    "env", env,
    bin, "--headless",
    "+lua assert(vim.lsp ~= nil, 'vim.lsp should load')",
    "+qa"
  })
  lu.assertEquals(handle:wait(), 0, "vim.lsp module should load")
end

function TestNvim:test_vim_pack_loads()
  local env = "VIMRUNTIME=" .. path.join(bin_dir, "share", "nvim", "runtime")
  local handle = spawn({
    "env", env,
    bin, "--headless",
    "+lua assert(vim.pack ~= nil, 'vim.pack should load')",
    "+qa"
  })
  lu.assertEquals(handle:wait(), 0, "vim.pack module should load")
end
