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

-- Wild menu (command completion)
opt.wildmenu = true
opt.wildmode = "longest:full,full"

-- Color scheme (syntax handled by treesitter)
-- Applied by mini.hues.setup() in mini.lua
vim.cmd.filetype("plugin indent on")