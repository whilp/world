-- type declarations for posix.dirent

local record dirent
  dir: function(path: string): {string}, string
  files: function(path: string): function(): string
end

return dirent
