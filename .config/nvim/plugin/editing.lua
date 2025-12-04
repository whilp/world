local opt = vim.opt
local map = vim.keymap.set
local opts = { silent = true }

-- Search settings
opt.hlsearch = false
opt.smartcase = true
opt.ignorecase = true
opt.gdefault = true  -- use 'g' flag by default with :s/foo/bar
opt.switchbuf = 'useopen,uselast'  -- use buffer in open windows when jumping

-- Path for fuzzy finding
opt.path:append("**")

-- Completion
opt.complete = "o,.,w,b,u"
opt.completeopt = { "fuzzy", "menuone", "noselect", "popup" }
opt.pumheight = 7
opt.pumwidth = 80

vim.api.nvim_create_autocmd("LspAttach", {
  callback = function()
    vim.opt_local.complete = "o"
  end,
})

-- Text formatting
opt.linebreak = true
opt.autoindent = true
opt.smartindent = false
opt.cindent = false

-- Tab settings
opt.expandtab = true
opt.shiftwidth = 2
opt.softtabstop = 2

-- Undo system
opt.undodir = os.getenv("HOME") .. "/.config/nvim/undodir"
opt.undofile = true

-- Clipboard operations
map({ "n", "v" }, "<Space>yy", '"+y', { desc = 'Yank to system clipboard' })
map("n", "<Space>pp", '"+p', { desc = 'Paste from system clipboard' })

-- File operations
map("n", "<Space>yf", function()
  vim.fn.setreg("@", vim.fn.expand("%:p"))
end, { noremap = true, silent = true, desc = 'Yank file path' })

map("n", "<Space>yu", function()
  vim.fn.setreg("+", vim.fn.expand("<cfile>"))
end, { desc = "Copy URL under cursor to clipboard" })

map("n", "<Space>gf", "<C-w>v gf", { desc = 'Open file under cursor in vertical split' })

-- Quick save
map("i", ",w", "<C-o>:write<CR>", { desc = 'Save file' })

-- Insert mode navigation
map("i", "<C-a>", "<C-o>^", { desc = 'Move to beginning of line' })
map("i", "<C-e>", "<C-o>$", { desc = 'Move to end of line' })

-- Enhanced editing shortcuts
map("n", "Q", "@@", { noremap = true, desc = 'Repeat last macro' })
map("n", "Y", "y$", { noremap = true, desc = 'Yank to end of line' })

-- Command aliases
vim.cmd("cnoreabbrev Q q")
vim.cmd("cnoreabbrev W noau w")

