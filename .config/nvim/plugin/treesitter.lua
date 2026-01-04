local lang = dofile(vim.fn.stdpath("config") .. "/parsers.lua")

local ok, ts = pcall(require, "nvim-treesitter")
if not ok then
  return
end

ts.setup({
  install_dir = vim.fn.stdpath("data") .. "/site",
})

local function is_parser_installed(name)
  local installed = ts.get_installed()
  return vim.tbl_contains(installed, name)
end

local function ensure_parser(name)
  if is_parser_installed(name) then
    return true
  end
  local install_ok = ts.install({ name }):wait()
  return install_ok
end

vim.api.nvim_create_autocmd("FileType", {
  pattern = lang,
  callback = function()
    local ft = vim.bo.filetype
    if not is_parser_installed(ft) then
      ensure_parser(ft)
    end
    local start_ok = pcall(vim.treesitter.start)
    if start_ok then
      vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
    end
  end,
})
