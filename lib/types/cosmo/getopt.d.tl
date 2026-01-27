-- type declarations for cosmo.getopt

local record Parser
  next: function(self): string, string
  remaining: function(self): {string}
end

local record getopt
  type LongOpt = {string, string, string}
  new: function(args: {string}, shortopts: string, longopts: {LongOpt}): Parser
end

return getopt
