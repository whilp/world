require("nvim-treesitter").setup({
  install_dir = vim.fn.stdpath("data") .. "/site",
})

local lang = {
  "python",
  "markdown",
  "bash",
  "lua",
  "yaml",
  "javascript",
  "json",
  "ruby",
  "go",
  "sql",
}

local ok, ts_configs = pcall(require, "nvim-treesitter.configs")
if ok then
  ts_configs.setup({
    ensure_installed = lang,
    highlight = {
      enable = true,
    },
    indent = {
      enable = true,
    },
  })

  vim.api.nvim_create_autocmd("FileType", {
    pattern = lang,
    callback = function()
      vim.treesitter.start()
    end,
  })
end
