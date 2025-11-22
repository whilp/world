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
      zsh = { "shfmt", "injected" },
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

vim.lsp.config.marksman = {
  cmd = { "marksman", "server" },
  filetypes = { "markdown", "markdown.mdx" },
  root_markers = { ".git", ".marksman.toml" },
}

vim.api.nvim_create_autocmd("FileType", {
  pattern = { "markdown", "markdown.mdx" },
  callback = function(args)
    vim.lsp.enable("marksman")
  end,
})
