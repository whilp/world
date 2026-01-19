-- luacheck ignore: neovim runtime
-- ast-grep ignore: neovim runtime
--check:false
-- nvim configuration entry point

local home = vim.fn.expand("~")

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

-- Load plugin/*.tl files (nvim only auto-sources .lua/.vim from plugin/)
-- Add config dir and plugin dir to package.path for teal loader
local config_dir = vim.fn.stdpath("config") as string
local plugin_dir = config_dir .. "/plugin"
package.path = config_dir .. "/?.tl;" .. config_dir .. "/?.lua;" .. plugin_dir .. "/?.tl;" .. plugin_dir .. "/?.lua;" .. package.path

if vim.fn.isdirectory(plugin_dir) == 1 then
  for _, file in ipairs(vim.fn.readdir(plugin_dir)) do
    if file:match("%.tl$") then
      local module = file:gsub("%.tl$", "")
      require(module)
    end
  end
end