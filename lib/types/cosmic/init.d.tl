-- type declarations for cosmic

local record Env
  stdout: FILE
  stderr: FILE
end

local type MainFn = function(args: {string}, env: Env): number, string

local record cosmic
  _VERSION: string
  _DESCRIPTION: string
  main: function(fn: MainFn)
end

return cosmic
