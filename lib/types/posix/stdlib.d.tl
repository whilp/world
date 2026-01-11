-- type declarations for posix.stdlib

local record stdlib
  mkstemp: function(template: string): number, string
  mkdtemp: function(template: string): string
  realpath: function(path: string): string, string
  getenv: function(name: string): string
  setenv: function(name: string, value: string, overwrite?: boolean): boolean, string
end

return stdlib
