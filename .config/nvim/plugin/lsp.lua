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

local ok, lspconfig = pcall(require, "lspconfig")
if ok then
  lspconfig.marksman.setup({})
end
