" github_dark_colorblind.vim - Neovim colorscheme matching Alacritty's dark theme
" Maintainer: Claude
" Last Change: 2025-07-23

" Reset highlighting
hi clear
if exists("syntax_on")
  syntax reset
endif

set background=dark
let g:colors_name = "github_dark_colorblind"

" Terminal colors from Alacritty theme
let s:bg = "#0d1117"
let s:fg = "#c9d1d9"
let s:cursor_bg = "#c9d1d9"
let s:cursor_fg = "#0d1117"
let s:black = "#484f58"
let s:red = "#ec8e2c"
let s:green = "#58a6ff"
let s:yellow = "#d29922"
let s:blue = "#58a6ff"
let s:magenta = "#bc8cff"
let s:cyan = "#39c5cf"
let s:white = "#b1bac4"
let s:bright_black = "#6e7681"
let s:bright_red = "#fdac54"
let s:bright_green = "#79c0ff"
let s:bright_yellow = "#e3b341"
let s:bright_blue = "#79c0ff"
let s:bright_magenta = "#bc8cff"
let s:bright_cyan = "#39c5cf"
let s:bright_white = "#b1bac4"

" Helper function
function! s:hi(group, fg, bg, attr)
  if !empty(a:fg)
    exec "hi " . a:group . " guifg=" . a:fg
  endif
  if !empty(a:bg)
    exec "hi " . a:group . " guibg=" . a:bg
  endif
  if !empty(a:attr)
    exec "hi " . a:group . " gui=" . a:attr
  endif
endfunction

" Editor colors
call s:hi("Normal", s:fg, s:bg, "")
call s:hi("LineNr", s:bright_black, "", "")
call s:hi("CursorLine", "", "#1d2229", "none")
call s:hi("CursorLineNr", s:bright_yellow, "", "bold")
call s:hi("CursorColumn", "", "#1d2229", "")
call s:hi("ColorColumn", "", "#1d2229", "")
call s:hi("SignColumn", s:fg, s:bg, "")
call s:hi("VertSplit", s:black, s:bg, "")
call s:hi("Folded", s:bright_black, "#1d2229", "")
call s:hi("FoldColumn", s:bright_black, s:bg, "")
call s:hi("IncSearch", s:bg, s:yellow, "none")
call s:hi("Search", s:bg, s:yellow, "none")
call s:hi("MatchParen", "", "#30363d", "bold")
call s:hi("StatusLine", s:fg, "#30363d", "none")
call s:hi("StatusLineNC", s:bright_black, "#21262d", "none")
call s:hi("WildMenu", s:bg, s:blue, "")
call s:hi("Title", s:green, "", "none")
call s:hi("Visual", "", "#30363d", "")
call s:hi("NonText", s:bright_black, "", "")
call s:hi("Comment", s:bright_black, "", "italic")
call s:hi("Pmenu", s:fg, "#1d2229", "none")
call s:hi("PmenuSel", s:bg, s:blue, "")
call s:hi("PmenuSbar", "", "#2d333b", "")
call s:hi("PmenuThumb", "", s:white, "")

" Standard syntax highlighting
call s:hi("Boolean", s:bright_yellow, "", "")
call s:hi("Character", s:bright_green, "", "")
call s:hi("Conditional", s:bright_red, "", "")
call s:hi("Constant", s:magenta, "", "")
call s:hi("Define", s:magenta, "", "none")
call s:hi("Error", s:red, s:bg, "bold")
call s:hi("Float", s:bright_yellow, "", "")
call s:hi("Function", s:blue, "", "")
call s:hi("Identifier", s:red, "", "none")
call s:hi("Include", s:blue, "", "")
call s:hi("Keyword", s:magenta, "", "")
call s:hi("Label", s:bright_yellow, "", "")
call s:hi("Number", s:bright_yellow, "", "")
call s:hi("Operator", s:magenta, "", "none")
call s:hi("PreProc", s:yellow, "", "")
call s:hi("Repeat", s:bright_red, "", "")
call s:hi("Special", s:cyan, "", "")
call s:hi("SpecialKey", s:bright_black, "", "")
call s:hi("Statement", s:bright_red, "", "")
call s:hi("StorageClass", s:yellow, "", "")
call s:hi("String", s:green, "", "")
call s:hi("Structure", s:yellow, "", "")
call s:hi("Tag", s:fg, "", "")
call s:hi("Todo", s:yellow, "bg", "")
call s:hi("Type", s:yellow, "", "none")
call s:hi("Typedef", s:yellow, "", "")

" Diff highlighting
call s:hi("DiffAdd", s:green, "#1d2229", "")
call s:hi("DiffChange", s:bright_black, "#1d2229", "")
call s:hi("DiffDelete", s:red, "#1d2229", "")
call s:hi("DiffText", s:blue, "#1d2229", "")
call s:hi("DiffAdded", s:green, "#1d2229", "")
call s:hi("DiffFile", s:red, "#1d2229", "")
call s:hi("DiffNewFile", s:green, "#1d2229", "")
call s:hi("DiffLine", s:blue, "#1d2229", "")
call s:hi("DiffRemoved", s:red, "#1d2229", "")

" Git highlighting
call s:hi("gitcommitOverflow", s:red, "", "")
call s:hi("gitcommitSummary", s:green, "", "")
call s:hi("gitcommitComment", s:bright_black, "", "")
call s:hi("gitcommitUntracked", s:bright_black, "", "")
call s:hi("gitcommitDiscarded", s:bright_black, "", "")
call s:hi("gitcommitSelected", s:bright_black, "", "")
call s:hi("gitcommitHeader", s:magenta, "", "")
call s:hi("gitcommitSelectedType", s:blue, "", "")
call s:hi("gitcommitUnmergedType", s:blue, "", "")
call s:hi("gitcommitDiscardedType", s:blue, "", "")
call s:hi("gitcommitBranch", s:yellow, "", "bold")
call s:hi("gitcommitUntrackedFile", s:cyan, "", "")
call s:hi("gitcommitUnmergedFile", s:red, "", "bold")
call s:hi("gitcommitDiscardedFile", s:red, "", "bold")
call s:hi("gitcommitSelectedFile", s:green, "", "bold")

" LSP and Diagnostic highlighting
call s:hi("LspDiagnosticsDefaultError", s:red, "", "")
call s:hi("LspDiagnosticsDefaultWarning", s:yellow, "", "")
call s:hi("LspDiagnosticsDefaultInformation", s:blue, "", "")
call s:hi("LspDiagnosticsDefaultHint", s:cyan, "", "")
call s:hi("LspDiagnosticsUnderlineError", s:red, "", "underline")
call s:hi("LspDiagnosticsUnderlineWarning", s:yellow, "", "underline")
call s:hi("LspDiagnosticsUnderlineInformation", s:blue, "", "underline")
call s:hi("LspDiagnosticsUnderlineHint", s:cyan, "", "underline")

" New Vim 0.8+ diagnostic highlighting
call s:hi("DiagnosticError", s:red, "", "")
call s:hi("DiagnosticWarn", s:yellow, "", "")
call s:hi("DiagnosticInfo", s:blue, "", "")
call s:hi("DiagnosticHint", s:cyan, "", "")
call s:hi("DiagnosticUnderlineError", s:red, "", "underline")
call s:hi("DiagnosticUnderlineWarn", s:yellow, "", "underline")
call s:hi("DiagnosticUnderlineInfo", s:blue, "", "underline")
call s:hi("DiagnosticUnderlineHint", s:cyan, "", "underline")