-- type declarations for posix.unistd

local record unistd
  rmdir: function(path: string): boolean, string
  getcwd: function(): string
  chdir: function(path: string): boolean, string
  access: function(path: string, mode: string): boolean, string
  readlink: function(path: string): string, string
  unlink: function(path: string): boolean, string
  link: function(oldpath: string, newpath: string): boolean, string
  symlink: function(target: string, linkpath: string): boolean, string
  getpid: function(): number
  getppid: function(): number
  getuid: function(): number
  getgid: function(): number
  gethostname: function(): string
  sleep: function(seconds: number): number
  exec: function(path: string, args: {string}): nil, string
  execp: function(file: string, args: {string}): nil, string
  fork: function(): number, string
  _exit: function(status: number)
end

return unistd
