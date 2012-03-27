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
set lbr
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

syntax enable
colorscheme bw

" Don't do paren matching.
let loaded_matchparen=1
let mapleader=","

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
noremap <silent> f :FufFile<CR>
noremap <silent> F :FufRenewCache<CR>:FufFile<CR>
noremap <Leader>p :set invpaste paste?<CR>
noremap <Leader>s :source ~/.vimrc<Enter>
noremap Q :x<Enter>
noremap <Leader>ci :w<Enter>:silent !hg ci<Enter><C-L>
noremap <Leader>cr :w<Enter>:silent !hg rec<Enter><C-L>
nmap <silent> <leader>s :set nolist!<CR>

cabbrev W w
cabbrev X x
cabbrev Q q
cabbrev Wq wq
cabbrev WQ wq
cabbrev E new

au FileType python  	setl sw=4 sts=4 ts=4 tw=79 ff=unix et
au FileType make    	setl ts=8 sts=0 noet
au FileType sshconfig 	setl sw=4 sts=4 ts=4 et
au FileType vim         setl sw=4 sts=4 ts=4 et
au FileType ruby        setl sw=4 sts=4 ts=4 et
