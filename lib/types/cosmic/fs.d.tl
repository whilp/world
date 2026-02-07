-- type declarations for cosmic.fs

local unix = require("cosmo.unix")

local type Stat = unix.Stat

local type Visitor = function(full_path: string, entry: string, stat: Stat, ctx: any): boolean

local record FileInfo
  mode: number
end

local record fs
  -- walk functions (formerly cosmic.walk)
  walk: function<T>(dir: string, visitor: Visitor, ctx?: T): T
  collect: function(dir: string, pattern: string): {string}
  collect_all: function(dir: string, base?: string, files?: {string:FileInfo}): {string:FileInfo}
  files: function(dir: string): function(): string, Stat

  -- path functions (formerly cosmic.path)
  dirname: function(path: string): string
  basename: function(path: string): string
  join: function(...: string): string
  exists: function(path: string): boolean
  isfile: function(path: string): boolean
  isdir: function(path: string): boolean
  islink: function(path: string): boolean

  -- new path functions
  normalize: function(path: string): string
  abspath: function(path: string): string
  relpath: function(path: string, base?: string): string
  splitext: function(path: string): string, string
  ext: function(path: string): string
end

return fs
