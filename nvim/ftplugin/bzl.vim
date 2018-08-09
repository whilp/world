function! RunBuildifier(buffer) abort
     let l:options = []
     let l:executable = "buildifier"

     return {
     \  'read_temporary_file': 0,
     \  'command': "buildifier -path %s",
     \}
endfunction

let b:ale_fixers = ['RunBuildifier']
let b:ale_linters = []
