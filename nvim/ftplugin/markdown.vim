function! RunMarkdownfmt(buffer) abort
    return {
                \ 'read_temporary_file': 0,
                \  'command': "markdownfmt %t",
                \}
endfunction

let b:ale_fixers = ['RunMarkdownfmt']
let b:ale_linters = []
