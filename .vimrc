set autoindent
set background=light
set backspace=indent,eol,start
set foldmethod=indent
set history=50
set ignorecase
set incsearch
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
set textwidth=68
set viminfo='200,f1
set whichwrap=h,l,~,[,]
set wildmenu
set wildmode=full

colorscheme bw

" Don't do paren matching.
let loaded_matchparen=1

" Tabs.
set noexpandtab
set shiftwidth=4
set softtabstop=4
set tabstop=8

" Use syntax highlighting.
syntax on

" Plugins.
runtime ftplugin/man.vim
filetype plugin indent on
filetype indent on
" Enable if we're using a version better than 7.0
if (v:version >= 700)
    " Spellcheck
    setlocal spell spelllang=en_us
    " Omnicompletion
    au Filetype * if exists('&omnifunc') && &omnifunc == "" |
    \ set ofu=syntaxcomplete#Complete |
    \ endif
endif

" Mappings.
inoremap <C-J> <Down>
inoremap <C-K> <Up>
nnoremap ; :
noremap Y y$
noremap [[ :diffget<CR>
noremap ]] :diffput<CR>
noremap F gw}

" Source vimrc.
map ,v :sp $VIMRC_

" (De|Re)indent.
map <silent> <C-D> <lt><lt>
map <silent> <C-T> >>

" Fix common mistakes on the command line.
cabbrev W w
cabbrev Wq wq
cabbrev Q q

" Use XHTML when generating HTML files.
let use_xhtml = 1
let html_use_css = 1

" Stuff for ada.vim.
let g:ada_default_compiler = 'gnat'
let g:mapleader = "<F12>"

" Auto-encrypt files.
augroup safe
    au!
    au BufReadPre,FileReadPre ~/.safe/* set viminfo=
    au BufReadPre,FileReadPre ~/.safe/* set noswapfile

    au VimLeave ~/.safe/* :!clear
augroup END

augroup filetypes
    au!
    au BufRead,BufNewFile *		if &ft == 'make' | set ts=8 sts=0 noexpandtab | endif
augroup END
