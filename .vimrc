call pathogen#infect()
call pathogen#helptags()

let g:Powerline_symbols = 'unicode'
let g:Powerline_colorscheme = 'skwp'

runtime ftplugin/man.vim

set autoindent
set background=dark
set backspace=indent,eol,start
set cursorline
set directory-=.
set directory+=.
set encoding=utf-8
set fileencodings=
set foldmethod=indent
set formatoptions+=2l
set hidden
set history=1000
set ignorecase
set incsearch
set indentexpr="-1"
set laststatus=2
set lbr
set listchars=tab:>-,trail:·,eol:$
set modeline
set modelines=1
set nobackup
set nocompatible
set nofoldenable
set nohls
set ruler
set scrolloff=2
set shortmess=aoOstTWAI
set showcmd
set showmatch
set smartcase
set statusline=%{fugitive#statusline()}\ %<%f\ %h%m%r%=%-14.(%l,%c%V%)\ %P
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc
set switchbuf=usetab
set tags+=./.tags;~/
set textwidth=80
set ttyfast
set viminfo='200,f1,n~/.vim/info
set whichwrap=h,l,~,[,]
set wildmenu
set wildmode=list:longest,full
set winheight=8
set winminheight=8

syntax enable
colorscheme solarized

" Don't do paren matching.
let loaded_matchparen=1
let mapleader=","

" In netrw, edit files in a new tab.
let g:netrw_browse_split=2
let g:netrw_liststyle=3

" Mappings.
map gf :tabnew <cfile><CR>
noremap <C-H> <C-W>h<C-W>_
noremap <C-J> <C-W>j<C-W>_
noremap <C-K> <C-W>k<C-W>_
noremap <C-L> <C-W>l<C-W>_
nnoremap ; :
nnoremap K <Nop>
noremap Y y$
noremap [[ :diffget<CR>
noremap ]] :diffput<CR>

nnoremap <Leader>S :source ~/.vimrc<Enter>
nnoremap <Leader>c :Gco -v<CR>
nnoremap <Leader>d :Gdiff<CR>
nnoremap <Leader>f :F<Space>
nnoremap <Leader>p :set invpaste paste?<CR>
nnoremap <Leader>q :x<CR>
nnoremap <Leader>s :FS<Space>
nnoremap <Leader>t :FT<Space>
nnoremap <Leader>v :FV<Space>

inoremap <C-B> <C-X>

call togglebg#map("<Leader>b")

cabbrev W w
cabbrev X x
cabbrev Q q
cabbrev Wq wq
cabbrev WQ wq
cabbrev E new
cabbrev cw botright cwindow

au BufRead,BufNewFile *.md		set filetype=markdown
au BufRead,BufNewFile *.ru		set filetype=ruby

au FileType python      setl sw=4 sts=4 ts=4 tw=79 ff=unix et omnifunc=pythoncomplete#Complete
au FileType make        setl ts=8 sts=0 noet
au FileType sshconfig   setl sw=4 sts=4 ts=4 et
au FileType vim         setl sw=4 sts=4 ts=4 et
au FileType sh          setl sw=4 sts=4 ts=4 et
au FileType ruby        setl sw=2 sts=2 ts=2 et
au FileType markdown    setl sw=2 sts=2 ts=2 et
au FileType yaml        setl sw=2 sts=2 ts=2 et
au FileType javascript  setl sw=2 sts=2 ts=2 et
au FileType html        setl sw=2 sts=2 ts=2 et

function! s:ExecuteInShell(command)
  let command = join(map(split(a:command), 'expand(v:val)'))
  let winnr = bufwinnr('^' . command . '$')
  silent! execute  winnr < 0 ? 'botright new ' . fnameescape(command) : winnr . 'wincmd w'
  setlocal buftype=nowrite bufhidden=wipe nobuflisted noswapfile nowrap number
  echo 'Execute ' . command . '...'
  silent! execute 'silent %!'. command
  silent! execute 'resize ' . line('$')
  silent! redraw
  silent! execute 'au BufUnload <buffer> execute bufwinnr(' . bufnr('#') . ') . ''wincmd w'''
  silent! execute 'nnoremap <silent> <buffer> <LocalLeader>r :call <SID>ExecuteInShell(''' . command . ''')<CR>'
  echo 'Shell command ' . command . ' executed.'
endfunction
command! -complete=shellcmd -nargs=+ Shell call s:ExecuteInShell(<q-args>)

augroup Git
  autocmd!
  autocmd FileType git set fdm=syntax
  " fold each file in a gitcommit used with the -v flag
  autocmd FileType gitcommit
        \ set fdm=expr foldexpr=getline(v:lnum)=~'^diff\\s--git'?'>1':'='
  " view the output of `git diff` in a vertical split
  command! -nargs=0 Gddiff
        \ vnew | exec "r!git diff --cached" | set ft=git
  " git status in a vertical split
  command! -nargs=0 Gsv Gst | wincmd H
augroup END
