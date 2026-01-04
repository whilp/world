local lang = dofile(vim.fn.stdpath("config") .. "/parsers.lua")

local ok, ts = pcall(require, "nvim-treesitter")
if not ok then
  return
end

ts.setup({
  install_dir = vim.fn.stdpath("data") .. "/site",
})

vim.api.nvim_create_autocmd("FileType", {
  pattern = lang,
  callback = function()
    vim.treesitter.start()
    vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
  end,
})
