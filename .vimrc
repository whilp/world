set autoindent
set background=light
set backspace=indent,eol,start
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
set listchars=tab:>-,trail:·,eol:$
set nobackup
set nocompatible
set nofoldenable
set nohls
set ruler
set scrolloff=3
set showcmd
set showmatch
set smartcase
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc
set tags+=./.tags;~/
set textwidth=80
set viminfo='200,f1
set whichwrap=h,l,~,[,]
set wildmenu
set wildmode=list:longest,full
set winheight=8
set winminheight=8

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
map <C-]> :tab split<CR>:exec("tjump ".expand("<cword>"))<CR>
map gf :tabnew <cfile><CR>
noremap <C-J> <C-W>j<C-W>_
noremap <C-K> <C-W>k<C-W>_
nnoremap ; :
nnoremap K <Nop>
noremap Y y$
noremap [[ :diffget<CR>
noremap ]] :diffput<CR>
noremap f :FufFile<CR>
noremap F gw}
noremap <Leader>p :set invpaste paste?<CR>
noremap <Leader>s :source ~/.vimrc<Enter>
noremap Q :x<Enter>
noremap <Leader>ci :w<Enter>:silent !hg ci<Enter><C-L>
noremap <Leader>cr :w<Enter>:silent !hg rec<Enter><C-L>
nmap <silent> <leader>s :set nolist!<CR>

abbreviate W w
abbreviate X x
abbreviate Q q
abbreviate Wq wq
abbreviate WQ wq
abbreviate E new

augroup filetypes
    au!
    au BufRead,BufNewFile *		if &ft == 'make' | set ts=8 sts=0 noexpandtab | endif
    au BufWritePost       ~/share/notes/** :silent !ctags --exclude=.tags -f ~/share/notes/.tags --language-force=notes -R ~/share/notes
    au BufWritePost       ~/share/todo/** :silent !ctags --exclude=.tags -f ~/share/todo/.tags --language-force=notes -R ~/share/todo
augroup END
