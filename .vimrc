set autoindent
set background=light
set backspace=indent,eol,start
set encoding=utf-8
set fileencodings=
set foldmethod=indent
set formatoptions+=2l
set history=50
set ignorecase
set incsearch
set indentexpr="-1"
set laststatus=2
set nobackup
set nocompatible
set nofoldenable
set nohls
set number
set ruler
set showcmd
set showmatch
set smartcase
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc
set textwidth=80
set viminfo='200,f1
set whichwrap=h,l,~,[,]
set wildmenu
set wildmode=full

syntax off

colorscheme bw

" Don't do paren matching.
let loaded_matchparen=1
let mapleader=","

" Tabs.
set expandtab
set shiftwidth=4
set softtabstop=4
set tabstop=4

" Mappings.
inoremap <C-J> <Down>
inoremap <C-K> <Up>
nnoremap ; :
nnoremap K <Nop>
noremap Y y$
noremap [[ :diffget<CR>
noremap ]] :diffput<CR>
noremap F gw}
noremap <Leader>p :set invpaste paste?<CR>
noremap <Leader>hc :w<Enter>:silent !hg ci<Enter><C-L>
noremap <Leader>hr :w<Enter>:silent !hg rec<Enter><C-L>

abbreviate W w
abbreviate X x
abbreviate Q q
abbreviate Wq wq
abbreviate WQ wq
abbreviate E new

augroup filetypes
    au!
    au BufRead,BufNewFile *		if &ft == 'make' | set ts=8 sts=0 noexpandtab | endif
augroup END
