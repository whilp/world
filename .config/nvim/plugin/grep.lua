local opt = vim.opt

-- Grep configuration
opt.grepprg = "rg --vimgrep --no-heading --smart-case"
opt.grepformat = "%f:%l:%c:%m,%f"

-- Enhanced grep commands
vim.cmd([[
function! Grep(...)
    return system(join([&grepprg] + [expandcmd(join(a:000, ' '))], ' '))
endfunction

command! -nargs=+ -complete=file_in_path -bar Grep  cgetexpr Grep(<f-args>)
command! -nargs=+ -complete=file_in_path -bar LGrep lgetexpr Grep(<f-args>)
augroup quickfix
    autocmd!
    autocmd QuickFixCmdPost cgetexpr cwindow
    autocmd QuickFixCmdPost lgetexpr lwindow
augroup END
]])

-- Command alias to redirect :grep to :Grep
vim.cmd('cnoreabbrev <expr> grep (getcmdtype() ==# ":" && getcmdline() ==# "grep") ? "Grep" : "grep"')
