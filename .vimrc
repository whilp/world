"#################  BEGIN HEADERS
" Filename	: $HOME/.vimrc
" Use		: configuration file for vim text editor
" CVS		: $Id: vimrc,v 1.53 2006/07/18 14:58:56 will Exp $
" Copyright	: Copyright (c) 2005, 2006 Will Maier
" License	: BSD; see <http://www.lfod.us/copyright.html>
"#################  END HEADERS

" UI settings.
colorscheme zenburn

set backspace=indent,eol,start
set foldmethod=indent
set history=50
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

" Don't do paren matching.
let loaded_matchparen=1

" Tabs.
set expandtab
set shiftwidth=4
set softtabstop=4
set tabstop=4

" Use syntax highlighting.
syntax enable

" Plugins.
runtime ftplugin/man.vim
filetype plugin indent on
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
