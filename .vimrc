set autoindent
set background=light
set backspace=indent,eol,start
set encoding=utf-8
set fileencodings=
set foldmethod=indent
set history=50
set ignorecase
set incsearch
set indentexpr="-1"
set laststatus=2
set nobackup
set nocompatible
set nofoldenable
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

colorscheme bw

" Don't do paren matching.
let loaded_matchparen=1

" Tabs.
set expandtab
set shiftwidth=4
set softtabstop=4
set tabstop=4

" Plugins.
filetype plugin indent on
filetype indent on

" Mappings.
inoremap <C-J> <Down>
inoremap <C-K> <Up>
nnoremap ; :
noremap Y y$
noremap [[ :diffget<CR>
noremap ]] :diffput<CR>
noremap F gw}

" (Un)expand a screenful of tabs.
map ;4 H!Lv 4Mj
map ;2 H!Lv 2Mj
map ;E H!Lv EMj
map ;e H!Lv eMj
map ;F {!}v f
map ;f {!}v f}
map ;s !lv s
map ;w :w
map ;x :x
map ;q :q

" Source vimrc.
map ,v :sp $VIMRC_

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
