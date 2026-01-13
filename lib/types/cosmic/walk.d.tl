-- type declarations for cosmic.walk

local unix = require("cosmo.unix")

local type Stat = unix.Stat

local type Visitor = function(full_path: string, entry: string, stat: Stat, ctx: any): boolean

local record FileInfo
  mode: number
end

local record walk
  walk: function<T>(dir: string, visitor: Visitor, ctx?: T): T
  collect: function(dir: string, pattern: string): {string}
  collect_all: function(dir: string, base?: string, files?: {string:FileInfo}): {string:FileInfo}
end

return walk
