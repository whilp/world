-- type declarations for cosmo.path

local record path
  join: function(...: string): string
  dirname: function(p: string): string
  basename: function(p: string): string
  exists: function(p: string): boolean
end

return path
