-- type declarations for posix.glob

local record glob
  glob: function(pattern: string, flags?: number): {string}
end

return glob
