local lu = require("luaunit")
local spawn = require("spawn")
local path = require("cosmo.path")

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
