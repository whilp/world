function! PythonJsonToolFix(buffer) abort
  return {
        \  'command': "python -m json.tool",
        \}
endfunction

function! PythonJsonToolLint(buffer) abort
  return {
        \ "command": "python -m json.tool",
        \ "output_stream": "stderr",
        \ "callback": "PythonJsonToolHandle",
        \}
endfunction

function! PythonJsonToolHandle(buffer, lines) abort
  " Handles messages like:
  "   Invalid control character at: line 37 column 29 (char 1405)
  "   Expecting , delimiter: line 38 column 13 (char 1418)

  let l:pattern = "\v^(.*): line (\d+) column (\d+)(.*)$"
  let l:output = []

  for l:match in ale#util#GetMatches(a:lines, l:pattern)
    call add(l:output, {
          \ 'lnum': l:match[2] + 0,
          \ 'col': l:match[3] + 0,
          \ 'text': l:match[1],
          \})
  endfor

  return l:output
endfunction

let b:ale_fixers = ['PythonJsonToolFix']
let b:ale_linters = ['PythonJsonToolLint']
