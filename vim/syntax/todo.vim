syn match HEADER "^[A-Z]\+$"
hi link HEADER PreProc

syn match HEADER2 "^ \+[A-Z]\+$"
hi link HEADER2 Constant

syn match DATE "\d\d\d\d.\d\d.\d\d"
hi link DATE Identifier

syn match COMMENT "# .*"
hi link COMMENT Comment

syn match DONE "DONE"
hi link DONE Errormsg

