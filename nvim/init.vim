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
set nohlsearch
set nomodeline
set nowrap
set scrollback=-1
set scrolloff=3
set shell=/bin/bash
set shiftwidth=4
set showmatch
set showtabline=0
set smartcase
set smarttab
set softtabstop=4
set splitbelow
set splitright
set tabstop=4
set textwidth=79
set visualbell
set wildmenu
set wildmode=list:longest
set completeopt-=preview
set completeopt+=longest,menuone,noselect
set shortmess+=c
set belloff+=ctrlg
let mapleader = " "

inoremap jj <ESC>
nnoremap <Leader>g :Rg<CR>
nnoremap <Leader>f :FZF<CR>
nnoremap <Leader>q :bp\|bd #<CR>
nnoremap <Leader>v :Gstatus<CR>
nnoremap <silent> [f :lprevious<CR>
nnoremap <silent> ]f :lnext<CR>
tnoremap jj <C-\><C-n>

autocmd BufRead,BufNewFile *.bzl setfiletype bzl
autocmd BufRead,BufNewFile BUILD setfiletype bzl
autocmd BufRead,BufNewFile *.BUILD setfiletype bzl
autocmd BufRead,BufNewFile BUILD.* setfiletype bzl
autocmd BufRead,BufNewFile WORKSPACE setfiletype bzl

" https://github.com/junegunn/fzf.vim#advanced-customization
command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
  \   <bang>0 ? fzf#vim#with_preview('up:60%')
  \           : fzf#vim#with_preview('right:50%:hidden', '?'),
  \   <bang>0)
let $FZF_DEFAULT_COMMAND = 'rg --files --smart-case'

let g:mucomplete#enable_auto_at_startup = 1
let g:mucomplete#delayed_completion = 1

let g:signify_disable_by_default = 1
let g:signify_vcs_list = ["git"]

" https://github.com/BurntSushi/ripgrep/issues/425#issuecomment-381446152

" TODO
" https://github.com/chriskempson/base16-vim
" https://github.com/mhinz/vim-grepper
" https://github.com/skywind3000/asyncrun.vim
packloadall
sil helptags ALL
