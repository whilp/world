local opt = vim.opt

-- File handling
opt.autoread = true
opt.hidden = true

-- Shell configuration
vim.o.shell = '/usr/bin/zsh'

vim.env.NVIM_INVIM = "true"
