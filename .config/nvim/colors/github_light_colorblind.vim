" github_light_colorblind.vim - Neovim colorscheme matching Alacritty's light theme
" Maintainer: Claude
" Last Change: 2025-07-23

" Reset highlighting
hi clear
if exists("syntax_on")
  syntax reset
endif

set background=light
let g:colors_name = "github_light_colorblind"

" Terminal colors from Alacritty theme
let s:bg = "#ffffff"
let s:fg = "#24292f"
let s:cursor_bg = "#24292f"
let s:cursor_fg = "#ffffff"
let s:black = "#24292f"
let s:red = "#b35900"
let s:green = "#0550ae"
let s:yellow = "#4d2d00"
let s:blue = "#0969da"
let s:magenta = "#8250df"
let s:cyan = "#1b7c83"
let s:white = "#6e7781"
let s:bright_black = "#57606a"
let s:bright_red = "#8a4600"
let s:bright_green = "#0969da"
let s:bright_yellow = "#633c01"
let s:bright_blue = "#218bff"
let s:bright_magenta = "#8250df"
let s:bright_cyan = "#1b7c83"
let s:bright_white = "#6e7781"

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
call s:hi("LineNr", s:white, "", "")
call s:hi("CursorLine", "", "#f6f8fa", "none")
call s:hi("CursorLineNr", s:yellow, "", "bold")
call s:hi("CursorColumn", "", "#f6f8fa", "")
call s:hi("ColorColumn", "", "#f6f8fa", "")
call s:hi("SignColumn", s:fg, s:bg, "")
call s:hi("VertSplit", s:white, s:bg, "")
call s:hi("Folded", s:bright_black, "#f6f8fa", "")
call s:hi("FoldColumn", s:bright_black, s:bg, "")
call s:hi("IncSearch", s:bg, s:blue, "none")
call s:hi("Search", s:bg, s:blue, "none")
call s:hi("MatchParen", "", "#dde1e6", "bold")
call s:hi("StatusLine", s:fg, "#dde1e6", "none")
call s:hi("StatusLineNC", s:bright_black, "#eaeef2", "none")
call s:hi("WildMenu", s:bg, s:blue, "")
call s:hi("Title", s:green, "", "none")
call s:hi("Visual", "", "#dde1e6", "")
call s:hi("NonText", s:white, "", "")
call s:hi("Comment", s:white, "", "italic")
call s:hi("Pmenu", s:fg, "#f6f8fa", "none")
call s:hi("PmenuSel", s:bg, s:blue, "")
call s:hi("PmenuSbar", "", "#dde1e6", "")
call s:hi("PmenuThumb", "", s:black, "")

" Standard syntax highlighting
call s:hi("Boolean", s:bright_yellow, "", "")
call s:hi("Character", s:green, "", "")
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
call s:hi("DiffAdd", s:green, "#e6ffec", "")
call s:hi("DiffChange", s:bright_black, "#eff1f3", "")
call s:hi("DiffDelete", s:red, "#ffebe9", "")
call s:hi("DiffText", s:blue, "#dbedff", "")
call s:hi("DiffAdded", s:green, "#e6ffec", "")
call s:hi("DiffFile", s:red, "#ffebe9", "")
call s:hi("DiffNewFile", s:green, "#e6ffec", "")
call s:hi("DiffLine", s:blue, "#dbedff", "")
call s:hi("DiffRemoved", s:red, "#ffebe9", "")

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