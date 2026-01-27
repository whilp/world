-- luacheck ignore: neovim runtime
--check:false
-- package.path is set up in init.tl to include config dir
local lang = require("parsers")

local ok, ts = pcall(require, "nvim-treesitter")
if not ok then
  return
end

-- Find bundled site directory (same logic as init.tl)
local version_dir = vim.fn.fnamemodify(vim.v.progpath, ":h:h")
local bundled_site = version_dir .. "/share/nvim/site"

ts.setup({
  install_dir = bundled_site,
})

vim.api.nvim_create_autocmd("FileType", {
  pattern = lang,
  callback = function()
    vim.treesitter.start()
    vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
  end,
})
