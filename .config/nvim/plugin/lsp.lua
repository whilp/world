vim.pack.add({
  { src = "https://github.com/neovim/nvim-lspconfig" },
  { src = "https://github.com/stevearc/conform.nvim", version = vim.version.range("9.0.0") },
})

require("conform")
local ok, conform = pcall(require, "conform")
if ok then
  conform.setup({
    formatters_by_ft = {
      lua = { "stylua" },
      python = { "ruff_format", "injected" },
      javascript = { "biome" },
      typescript = { "biome" },
      javascriptreact = { "biome" },
      typescriptreact = { "biome" },
      json = { "biome" },
      jsonc = { "biome" },
      css = { "biome" },
      html = { "superhtml", "injected" },
      graphql = { "biome" },
      sql = { "sqruff" },
      sh = { "shfmt", "injected" },
      bash = { "shfmt", "injected" },
      markdown = { "comrak", "injected" },
    },
    formatters = {
      comrak = {
        inherit = false,
        command = "comrak-fmt",
        args = {},
      },
    },
    format_on_save = {
      timeout_ms = 500,
      lsp_format = "fallback",
      stop_after_first = false,
    },
  })
end

local ok, lspconfig = pcall(require, "lspconfig")
if ok then
  lspconfig.ast_grep.setup({
    cmd = { "ast-grep", "lsp" },
    filetypes = {
      "go",
      "java",
      "python",
      "javascript",
      "typescript",
      "html",
      "css",
      "lua",
    },
    root_dir = lspconfig.util.root_pattern("sgconfig.yaml", "sgconfig.yml", ".git"),
  })

  lspconfig.marksman.setup({})
end
