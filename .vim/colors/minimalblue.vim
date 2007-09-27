" Vim color file
" Maintainer:   Marco Squarcina <lavish@gmail.com>
" Last Change:  2007/09/25
" Based On:	http://fugal.net/vim/colors/bw.vim

" cool help screens
" :he group-name
" :he highlight-groups
" :he cterm-colors
" :he highlight

if version > 580
    " no guarantees for version 5.8 and below, but this makes it stop
    " complaining
    hi clear
    if exists("syntax_on")
	syntax reset
    endif
endif
let g:colors_name="minimalblue"

hi SpecialKey     cterm=bold		ctermfg=NONE
hi NonText        cterm=bold	    	ctermfg=NONE
hi Directory      cterm=bold	    	ctermfg=NONE
hi ErrorMsg       ctermbg=NONE		ctermfg=red
hi IncSearch      cterm=reverse	    	ctermfg=NONE
hi Search         cterm=underline    	ctermbg=3
hi MoreMsg        cterm=bold	    	ctermfg=NONE
hi ModeMsg        cterm=bold	    	ctermfg=NONE
hi LineNr         cterm=standout   	ctermfg=7
hi Question       cterm=standout    	ctermfg=NONE
hi StatusLine     cterm=standout	ctermfg=7	ctermbg=0
hi StatusLineNC   cterm=reverse 	ctermfg=7	ctermbg=0
hi VertSplit      cterm=standout	ctermfg=7
hi Title          cterm=bold		ctermfg=NONE
hi Visual	  cterm=reverse		ctermfg=2
hi VisualNOS      cterm=bold,underline	ctermfg=NONE
hi WarningMsg     cterm=standout	ctermfg=NONE
hi WildMenu       cterm=standout	ctermfg=NONE
hi Folded         cterm=standout	ctermfg=NONE
hi FoldColumn     cterm=standout	ctermfg=gray	ctermbg=0
hi DiffAdd        cterm=bold	        ctermfg=NONE
hi DiffChange     cterm=bold	        ctermfg=NONE
hi DiffDelete     cterm=bold	        ctermfg=NONE
hi DiffText       cterm=reverse	        ctermfg=NONE
hi Comment        cterm=none		ctermfg=4
hi Constant       			ctermfg=NONE
hi Special        cterm=bold		ctermfg=NONE
hi Identifier     cterm=bold		ctermfg=NONE
hi Statement      cterm=bold		ctermfg=NONE
hi PreProc 				ctermfg=NONE
hi Type           cterm=bold		ctermfg=NONE
hi Underlined     cterm=underline	ctermfg=NONE
hi Ignore         cterm=bold		ctermfg=NONE
hi Error          ctermbg=NONE		ctermfg=1
hi Todo           cterm=standout	ctermfg=NONE
