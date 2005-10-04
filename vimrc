"#################  BEGIN HEADERS
" Filename	: $HOME/.vimrc
" Use		: configuration file for vim text editor
" Author	: Will Maier <willmaier@ml1.net>
" Updated	: 2005.09.27 12:00:21 -0500
"#################  END HEADERS

" --[ SET OPTIONS
    set nocompatible			" Use Vim defaults instead of 100% vi compatibility
    set backspace=indent,eol,start	" more powerful backspacing
    set backup
    set backupdir=$HOME/.vimfiles/

    set autoindent			" always set autoindenting on
    " set linebreak			" Don't wrap words by default
    set textwidth=78			" Don't wrap lines by default 
    set nobackup			" Don't keep a backup file
    set viminfo='200,f1			" read/write a .viminfo file, don't store more than
					" 50 lines of registers
    set history=50		    	" keep 50 lines of command line history
    set ruler				" show the cursor position all the time
    set number				" show line numbers
    set tabstop=8
    set softtabstop=4
    set shiftwidth=4
    set smartindent
    set foldmethod=indent		" Fold based on line indent
    set nofoldenable
    set wildmenu			" tab completed menus
    set wildmode=full
    set whichwrap=h,l,~,[,]		" Allow characters to wrap lines
    

    " Suffixes that get lower priority when doing tab completion for filenames.
    " These are files we are not likely to want to edit or read.
    set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc

    " We know xterm-debian is a color terminal
    if &term =~ "xterm-debian" || &term =~ "xterm-xfree86"
	set t_Co=16
	set t_Sf=[3%dm
	set t_Sb=[4%dm
    endif

    " Make p in Visual mode replace the selected text with the "" register.
    vnoremap p <Esc>:let current_reg = @"<CR>gvdi<C-R>=current_reg<CR><Esc>

    " spellcheck
    noremap <C-S> :so `vimspell.sh %`<CR><CR>
    noremap <C-A> :syntax clear SpellErrors<CR>

    " use for notetaking
    map <silent> <C-E> :s/*/ / <CR>$
    imap <silent> <C-E> <Esc>:s/*/ / <CR>A

    " save shortcut
    imap <C-W> <Esc>:w<CR>a

    " Fix my shitty typing
    abbr hte the
    abbr teh the

    " Vim5 and later versions support syntax highlighting. Uncommenting the next
    " line enables syntax highlighting by default.
    syntax on

    " If using a dark background within the editing area and syntax highlighting
    " turn on this option as well
    set background=dark

    " The following are commented out as they cause vim to behave a lot
    " different from regular vi. They are highly recommended though.
    set showcmd		" Show (partial) command in status line.
    set showmatch		" Show matching brackets.
    set ignorecase		" Do case insensitive matching
    set incsearch		" Incremental search
    set autowrite		" Automatically save before commands like :next and :make

" --[ KEY BINDINGS
"    noremap <Space> <PageDown>		" Allow space to page down
    noremap <F1>			" Turn off the help dialog (I hope)
"    noremap <Ins> 2<C-Y>		" Scroll window without moving cursor
"    noremap <Del> 2<C-E>
"    noremap <PageDown>
"    noremap <PageUp>
    noremap Y y$

" --[ AUTOCMD
    if has("autocmd")
	" Enabled file type detection
	" Use the default filetype settings. If you also want to load indent files
	" to automatically do language-dependent indenting add 'indent' as well.
	filetype plugin indent on

augroup date
    au!
    au BufWrite         *      silent! execute '1,10g/^.\? Updated.*:/s/:.*/: ' . strftime("%Y.%m.%d %H:%M:%S %z") . '/'
    au BufWrite		*      silent! execute 'g;'
augroup END

" --[ LATEX-SPECIFIC
augroup latex
    au!
    au BufReadPre,FileReadPre	    *.tex   set fo=tcqwal
augroup END

" --[ AUTO-ENCRYPT FILES
	augroup gnupg
	    au!
	    autocmd BufReadPre,FileReadPre	   *.gpg,*.asc set viminfo=""
	    autocmd BufReadPre,FileReadPre	   *.gpg,*.asc set noswapfile
	    autocmd BufReadPre,FileReadPre	   *.gpg,*.asc set foldclose=all
	    autocmd BufReadPre,FileReadPre	   *.gpg,*.asc set foldmethod=indent
	    autocmd BufReadPre,FileReadPre	   *.gpg,*.asc set
	    autocmd BufReadPre,FileReadPre    *.gpg,*.asc set bin
	    autocmd BufReadPre,FileReadPre    *.gpg,*.asc let ch_save = &ch|set ch=2
	    autocmd BufReadPost,FileReadPost  *.gpg,*.asc 1;'[,']!gpg -d 2>/dev/null
	    autocmd BufReadPost,FileReadPost  *.gpg,*.asc set nobin
	    autocmd BufReadPost,FileReadPost  *.gpg,*.asc execute ":doautocmd BufReadPost " . expand("%:r")
	    autocmd BufReadPost,FileReadPost  *.gpg,*.asc let &ch = ch_save|unlet ch_save

	    autocmd BufWritePre,FileWritePre  *.gpg,*.asc set bin
	    autocmd BufWritePre,FileWritePre  *.gpg 1;'[,']!gpg -o - -c --cipher-algo=blowfish --force-mdc
	    autocmd BufWritePre,FileWritePre  *.asc 1;'[,']!gpg -a -o - -c --cipher-algo=blowfish --force-mdc
	    autocmd BufWritePost,FileWritePost    *.gpg,*.asc set nobin
	    autocmd BufWritePost,FileWritePost    *.gpg,*.asc undo

	    autocmd FileAppendPre         *.gpg,*.asc !gpg -d <afile> 2>/dev/null > <afile>:r
	    autocmd FileAppendPre         *.gpg,*.asc !mv <afile>:r <afile>
	    autocmd FileAppendPost        *.gpg,*.asc !mv <afile> <afile>:r
	    autocmd FileAppendPost        *.gpg !gpg -o <afile> -c --cipher-algo=blowfish --force-mdc <afile>:r 
	    autocmd FileAppendPost        *.asc !gpg -a -o <afile> -c --cipher-algo=blowfish --force-mdc <afile>:r 
	    autocmd FileAppendPost        *.gpg,*.asc !rm <afile>:r
	endif " has ("autocmd")

    " Source a global configuration file if available
    if filereadable("/etc/vim/vimrc.local")
	"  source /etc/vim/vimrc.local
    endif
