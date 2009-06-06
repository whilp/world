set background=light
highlight clear

if exists("syntax_on")
	syntax reset
endif
let g:colors_name="bw"

hi Comment		cterm=bold				ctermfg=NONE	ctermbg=NONE
hi Constant		cterm=NONE				ctermfg=NONE	ctermbg=NONE
hi diffFile		cterm=NONE				ctermfg=NONE	ctermbg=NONE
hi DiffAdd		cterm=reverse			ctermfg=NONE	ctermbg=NONE
hi DiffChange	cterm=reverse			ctermfg=NONE	ctermbg=NONE
hi DiffDelete	cterm=reverse			ctermfg=NONE	ctermbg=NONE
hi DiffText		cterm=reverse,underline	ctermfg=NONE	ctermbg=NONE
hi Directory	cterm=bold				ctermfg=NONE	ctermbg=NONE
hi Error		cterm=reverse			ctermfg=NONE	ctermbg=NONE
hi ErrorMsg		cterm=standout			ctermfg=NONE	ctermbg=NONE
hi FoldColumn	cterm=NONE				ctermfg=NONE	ctermbg=NONE
hi Folded		cterm=NONE				ctermfg=NONE	ctermbg=NONE
hi Identifier	cterm=underline			ctermfg=NONE	ctermbg=NONE
hi Ignore		cterm=bold				ctermfg=NONE	ctermbg=NONE
hi IncSearch	cterm=reverse			ctermfg=NONE	ctermbg=NONE
hi LineNr		cterm=NONE				ctermfg=NONE	ctermbg=NONE
hi ModeMsg		cterm=bold				ctermfg=NONE	ctermbg=NONE
hi MoreMsg		cterm=bold				ctermfg=NONE	ctermbg=NONE
hi NonText		cterm=bold				ctermfg=NONE	ctermbg=NONE
hi PreProc		cterm=NONE				ctermfg=NONE	ctermbg=NONE
hi Question		cterm=standout			ctermfg=NONE	ctermbg=NONE
hi Search		cterm=reverse			ctermfg=NONE	ctermbg=NONE
hi Special		cterm=bold				ctermfg=NONE	ctermbg=NONE
hi SpecialKey	cterm=bold				ctermfg=NONE	ctermbg=NONE
hi Statement	cterm=bold				ctermfg=NONE	ctermbg=NONE
hi StatusLine	cterm=bold,reverse		ctermfg=NONE	ctermbg=NONE
hi StatusLineNC	cterm=reverse			ctermfg=NONE	ctermbg=NONE
hi Title		cterm=bold				ctermfg=NONE	ctermbg=NONE
hi Todo			cterm=standout			ctermfg=NONE	ctermbg=NONE
hi Type			cterm=NONE				ctermfg=NONE	ctermbg=NONE
hi Underlined	cterm=underline			ctermfg=NONE	ctermbg=NONE
hi VertSplit	cterm=reverse			ctermfg=NONE	ctermbg=NONE
hi Visual		cterm=reverse			ctermfg=NONE	ctermbg=NONE
hi VisualNOS	cterm=bold,underline	ctermfg=NONE	ctermbg=NONE
hi WarningMsg	cterm=standout			ctermfg=NONE	ctermbg=NONE
hi WildMenu		cterm=standout			ctermfg=NONE	ctermbg=NONE
