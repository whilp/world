--check:false
-- nvim configuration entry point

-- Add lua paths
local home = vim.fn.expand("~")
package.path = home .. "/lib/?.lua;" .. home .. "/lib/3p/?.lua;" .. package.path

-- Add bundled site directory (relative to nvim binary) to packpath
local version_dir = vim.fn.fnamemodify(vim.v.progpath, ":h:h")
local bundled_site = version_dir .. "/share/nvim/site"
if vim.fn.isdirectory(bundled_site) == 1 then
  vim.opt.packpath:prepend(bundled_site)
end

-- Add extras to runtimepath if it exists
local extras_nvim = home .. "/extras/nvim"
if vim.fn.isdirectory(extras_nvim) == 1 then
  vim.opt.runtimepath:append(extras_nvim)
end

-- Load bundled plugins
vim.cmd.packadd('mini.nvim')
vim.cmd.packadd('nvim-lspconfig')
vim.cmd.packadd('conform.nvim')
vim.cmd.packadd('nvim-treesitter')