filetype on
filetype plugin indent on
syntax on

set autoindent
set autoread
set backspace=indent,eol,start
set expandtab
set formatoptions=qrn1
set grepprg=rg\ --vimgrep\ --no-heading\ --smart-case
set hidden
set ignorecase
set incsearch
set nomodeline
set nowrap
set scrollback=-1
set scrolloff=3
set shell=/bin/bash
set shiftwidth=4
set showmatch
set showtabline=0
set smartcase
set softtabstop=4
set tabstop=4
set textwidth=79
set visualbell
set wildmenu
set wildmode=list:longest
set wrap
let mapleader = " "

inoremap jj <ESC>
nnoremap ; :
nnoremap <Leader>g :silent! grep<Space>
nnoremap <Leader>q :bp\|bd #<CR>
nnoremap <leader>s :noh<CR>
nnoremap <silent> [f :lprevious<CR>
nnoremap <silent> ]f :lnext<CR>
tnoremap jj <C-\><C-n>

autocmd BufRead,BufNewFile *.bzl,BUILD,*.BUILD,BUILD.*,WORKSPACE setfiletype bzl


" https://github.com/BurntSushi/ripgrep/issues/425#issuecomment-381446152

" TODO
" https://github.com/vim-airline/vim-airline
" https://github.com/justinmk/vim-sneak
" https://github.com/chriskempson/base16-vim
" https://github.com/mhinz/vim-grepper
" https://github.com/skywind3000/asyncrun.vim
" https://github.com/vim-ctrlspace/vim-ctrlspace
" https://github.com/jreybert/vimagit
" https://github.com/w0rp/ale
packloadall
sil helptags ALL
