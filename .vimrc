"#################  BEGIN HEADERS
" e Filename	: $HOME/.vimrc
" Use		: configuration file for vim text editor
" CVS		: $Id: vimrc,v 1.53 2006/07/18 14:58:56 will Exp $
" Copyright	: Copyright (c) 2005, 2006 Will Maier
" License	: BSD; see <http://www.lfod.us/copyright.html>
"#################  END HEADERS

" Options.
set autoindent
set autowrite
set background=dark
set backspace=indent,eol,start
set expandtab
set foldmethod=indent
set history=50
set incsearch
set laststatus=2
set nobackup
set nocompatible
set nofoldenable
set number
set ruler
set shiftwidth=4
set showcmd
set showmatch
set showtabline=2
set smartcase
set softtabstop=4
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc
set tabstop=4
set textwidth=68
set viminfo='200,f1
set whichwrap=h,l,~,[,]
set wildmenu
set wildmode=full

let loaded_matchparen=1
let use_xhtml = 1
let html_use_css = 1

" Plugins.
runtime ftplugin/man.vim
filetype plugin indent on
syntax on
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
map ,v :sp $VIMRC_
map <silent> ,V :source ~/.vimrc:filetype detect:exe ":echo 'vimrc reloaded'"
map <silent> <C-D> <lt><lt>
map <silent> <C-T> >>
nnoremap ; :
noremap Y y$
vnoremap ; :
vnoremap p <Esc>:let current_reg = @"<CR>gvdi<C-R>=current_reg<CR><Esc>

" Fix my shitty typing
abbr hte the
abbr teh the
abbr th the
abbr iwth with
abbr wiht with
abbr hteir their
abbr rae are

" Fix my fat fingers
cabbrev W w
cabbrev Wq wq
cabbrev Q q

" Auto-encrypt files.
augroup safe
    au!
    au BufReadPre,FileReadPre *.safe set viminfo=
    au BufReadPre,FileReadPre *.safe set noswapfile

    au BufReadPost,FileReadPost *.safe set bin
    au BufReadPost,FileReadPost *.safe :%!openssl bf -d -a 2>/dev/null
    au BufReadPost,FileReadPost *.safe |redraw!
    au BufReadPost,FileReadPost *.safe set nobin

    au BufWritePre,FileWritePre *.safe set bin
    au BufWritePre,FileWritePre *.safe :%!openssl bf -salt -a 2>/dev/null
    au BufWritePre,FileWritePre *.safe set nobin
    au BufWritePost,FileWritePost *.safe undo
    au BufWritePost,FileWritePost *.safe |redraw!

    au VimLeave *.safe :!clear
augroup END
