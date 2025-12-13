-- nvim configuration entry point

-- Add lua paths
local home = vim.fn.expand("~")
package.path = home .. "/.local/lib/lua/?.lua;" .. home .. "/.local/lib/lua/3p/?.lua;" .. package.path

-- Add extras to runtimepath if it exists
local extras_nvim = home .. "/extras/nvim"
if vim.fn.isdirectory(extras_nvim) == 1 then
  vim.opt.runtimepath:append(extras_nvim)
end

-- Load plugins before anything else
vim.pack.add({
  { src = "https://github.com/nvim-mini/mini.nvim" },
})


vim.pack.add({
  { src = "https://github.com/nvim-lua/plenary.nvim" },
  { src = "https://github.com/NeogitOrg/neogit" },
})

vim.pack.add({
  { src = "https://github.com/neovim/nvim-lspconfig" },
  { src = "https://github.com/stevearc/conform.nvim", version = vim.version.range("9.1.0") },
})

vim.pack.add({
  { src = "https://github.com/nvim-treesitter/nvim-treesitter", branch = "main" },
})