local lang = dofile(vim.fn.stdpath("config") .. "/parsers.lua")

local ok, ts = pcall(require, "nvim-treesitter")
if ok then
  ts.setup({
    install_dir = vim.fn.stdpath("data") .. "/site",
    ensure_install = lang,
  })
end

vim.api.nvim_create_autocmd("FileType", {
  pattern = lang,
  callback = function()
    vim.treesitter.start()
    vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
  end,
})
