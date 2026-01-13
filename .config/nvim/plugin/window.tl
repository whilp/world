-- luacheck ignore: neovim runtime
local opt = vim.opt
local map = vim.keymap.set

-- Window settings
opt.splitright = true
opt.splitbelow = true
opt.winminheight = 0
opt.winborder = "rounded"

-- Move between windows
map({ "n", "i", "v", "t" }, "<D-h>", "<cmd>wincmd h<cr>", { desc = "Move left" })
map({ "n", "i", "v", "t" }, "<D-j>", "<cmd>wincmd j<cr>", { desc = "Move down" })
map({ "n", "i", "v", "t" }, "<D-k>", "<cmd>wincmd k<cr>", { desc = "Move up" })
map({ "n", "i", "v", "t" }, "<D-l>", "<cmd>wincmd l<cr>", { desc = "Move right" })

-- Split and move
map({ "n", "i", "v", "t" }, "<D-S-h>", "<cmd>leftabove vsplit<cr>", { desc = "Split left and move" })
map({ "n", "i", "v", "t" }, "<D-S-j>", "<cmd>belowright split<cr>", { desc = "Split down and move" })
map({ "n", "i", "v", "t" }, "<D-S-k>", "<cmd>leftabove split<cr>", { desc = "Split up and move" })
map({ "n", "i", "v", "t" }, "<D-S-l>", "<cmd>belowright vsplit<cr>", { desc = "Split right and move" })

-- Buffer management
map("n", "<D-q>", "<cmd>enew|bd #<cr>", { noremap = true, silent = true, desc = "Close current buffer" })
