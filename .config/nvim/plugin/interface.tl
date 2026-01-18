-- luacheck ignore: neovim runtime
local opt = vim.opt

-- Visual and UI settings
opt.termguicolors = true
opt.list = true
opt.lazyredraw = true
opt.inccommand = 'nosplit'
opt.title = true
opt.conceallevel = 2
opt.showtabline = 2
opt.signcolumn = 'yes'
opt.laststatus = 2  -- always show statusline
opt.updatetime = 250

-- Shorten message formats
opt.shortmess:append('c')  -- completion messages
opt.shortmess:append('I')  -- hide startup screen
opt.shortmess:append('A')  -- ignore swap file messages
opt.shortmess:append('a')  -- shorter message formats

-- Set terminal title with host identifier
local whereami = vim.env.WHEREAMI
if whereami then
  opt.titlestring = whereami
end

-- Wild menu (command completion)
opt.wildmenu = true
opt.wildmode = "longest:full,full"

-- Hide cmdline when not in use
opt.cmdheight = 0

local cmdline_group = vim.api.nvim_create_augroup("cmdline_height", { clear = true })

local function set_cmdheight(val: integer)
  if opt.cmdheight:get() ~= val then
    opt.cmdheight = val
    vim.cmd.redrawstatus()
  end
end

vim.api.nvim_create_autocmd("CmdlineEnter", {
  group = cmdline_group,
  callback = function()
    if vim.fn.getcmdtype() == ":" then
      set_cmdheight(1)
    end
  end,
})

vim.api.nvim_create_autocmd("CmdlineChanged", {
  group = cmdline_group,
  callback = function()
    if vim.fn.getcmdtype() == ":" and vim.fn.getcmdline() == "" then
      set_cmdheight(0)
    end
  end,
})

vim.api.nvim_create_autocmd("CmdlineLeave", {
  group = cmdline_group,
  callback = function()
    set_cmdheight(0)
  end,
})

-- Color scheme (syntax handled by treesitter)
-- Applied by mini.hues.setup() in mini.lua
vim.cmd.filetype("plugin indent on")