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
