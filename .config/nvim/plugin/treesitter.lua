-- Use built-in treesitter for syntax highlighting
-- Neovim 0.12+ has built-in treesitter support with many parsers included

-- Enable treesitter highlighting globally
vim.opt.syntax = 'off'  -- Disable legacy syntax highlighting

-- Auto-start treesitter for files with available parsers
vim.api.nvim_create_autocmd({'BufRead', 'BufNewFile'}, {
  callback = function()
    local lang = vim.treesitter.language.get_lang(vim.bo.filetype)
    if lang then
      local ok = pcall(vim.treesitter.language.add, lang)
      if ok then
        pcall(vim.treesitter.start)
      end
    end
  end,
})