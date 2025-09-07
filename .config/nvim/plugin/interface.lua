local opt = vim.opt

-- Visual and UI settings
opt.list = true
opt.lazyredraw = true
opt.inccommand = 'nosplit'
opt.title = true
opt.conceallevel = 2
opt.showtabline = 2

-- Wild menu (command completion)
opt.wildmenu = true
opt.wildmode = "longest:full,full"

-- Color scheme (syntax handled by treesitter)
vim.cmd('colorscheme github_dark_colorblind')
vim.cmd.filetype("plugin indent on")