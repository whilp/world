" Only run if we haven't run before.
if exists("b:did_ftplugin")
    finish
endif
let b:did_ftplugin = 1

" TODO foldexpr function.
function todo#TodoFoldExpr(Line)
    let l:Level = ""
    let l:PrevIndent = indent(a:Line - 1)
    let l:ThisIndent = indent(a:Line)
    let l:NextIndent = indent(a:Line + 1)

    if l:NextIndent > l:ThisIndent
        " Fold starts here.
        let l:Level = ">"
    elseif l:NextIndent < l:ThisIndent
        " Fold ends here.
        let l:Level = "<"
    endif

    if l:ThisIndent > l:PrevIndent
        let l:Level .= "="
    elseif l:NextIndent > l:ThisIndent
        let l:Level .= l:NextIndent/&shiftwidth
    else
        let l:Level .= l:ThisIndent/&shiftwidth
    endif

    return l:Level
endfunction

setlocal foldmethod=expr
setlocal foldexpr=todo#TodoFoldExpr(v:lnum)
setlocal foldlevel=0
