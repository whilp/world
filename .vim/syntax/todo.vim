" Only run if we haven't run before.
if exists("b:did_ftplugin")
    finish
endif
let b:did_ftplugin = 1

" TODO foldexpr function.
function todo#TodoFoldExpr(Line)
    let l:ThisLine = getline(a:Line)
    let l:NextLine = getline(a:Line + 1) 

    if indent(a:Line) > indent(a:Line - 1)
        let l:Level = "="
    elseif indent(a:Line + 1) > indent(a:Line)
        let l:Level = indent(a:Line + 1)/&shiftwidth
    else
        let l:Level = indent(a:Line)/&shiftwidth
    endif

    return l:Level
endfunction

setlocal foldmethod=expr
setlocal foldexpr=todo#TodoFoldExpr(v:lnum)
setlocal foldlevel=0
