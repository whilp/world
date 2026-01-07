-- luacheck ignore: neovim runtime
local opt = vim.opt

-- Grep configuration
opt.grepprg = "rg --vimgrep --no-heading --smart-case"
opt.grepformat = "%f:%l:%c:%m,%f"


-- Command alias to redirect :grep to :Grep
vim.cmd('cnoreabbrev <expr> grep (getcmdtype() ==# ":" && getcmdline() ==# "grep") ? "Grep" : "grep"')

-- Rebind :Grep to :Pick grep_live
vim.cmd('command! -nargs=* Grep Pick grep_live <args>')
