-- type declarations for posix

local record posix
  mkdir: function(path: string): boolean, string
end

return posix
