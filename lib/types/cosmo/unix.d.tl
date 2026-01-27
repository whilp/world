-- type declarations for cosmo.unix

local record Stat
  mode: function(self): number
  size: function(self): number
  mtim: function(self): number
end

local record DirHandle
  read: function(self): string
  close: function(self)
  -- Allow iteration: for entry in handle do
  metamethod __call: function(self: DirHandle): string
end

local record unix
  -- process control
  fork: function(): number
  execve: function(path: string, argv: {string}, env: {string})
  execvp: function(cmd: string, argv: {string})
  execvpe: function(cmd: string, argv: {string}, env: {string})
  wait: function(pid?: number): number, number
  exit: function(code: number)
  kill: function(pid: number, signal: number): boolean
  getpid: function(): number
  daemon: function(nochdir: boolean, noclose: boolean): boolean

  -- pipes
  pipe: function(): number, number
  dup: function(fd: number, newfd?: number): number

  -- file operations
  open: function(path: string, flags: number, mode?: number): number, string
  close: function(fd: number): boolean
  read: function(fd: number, size: number): string
  write: function(fd: number, data: string): number
  lseek: function(fd: number, offset: number, whence: number): number
  ftruncate: function(fd: number, length: number): boolean

  -- directory operations
  opendir: function(path: string): DirHandle
  mkdir: function(path: string, mode: number): boolean
  makedirs: function(path: string, mode?: number): boolean, string
  mkdtemp: function(template: string): string
  rmrf: function(path: string): boolean
  rename: function(src: string, dst: string): boolean
  unlink: function(path: string): boolean
  symlink: function(target: string, link_path: string): boolean
  readlink: function(path: string): string

  -- path/directory functions
  getcwd: function(): string
  chdir: function(path: string): boolean
  realpath: function(path: string): string
  access: function(path: string, mode: number): boolean
  chmod: function(path: string, mode: number): boolean
  stat: function(path: string, flags?: number): Stat

  -- environment
  environ: function(): {string}
  setenv: function(name: string, value: string): boolean
  unsetenv: function(name: string): boolean
  commandv: function(cmd: string): string
  gethostname: function(): string

  -- time
  nanosleep: function(sec: number, nsec: number)

  -- network
  socket: function(family: number, socktype: number): number
  connect: function(fd: number, path: string): boolean

  -- signals
  sigaction: function(signal: number, handler: function())

  -- file locking
  fcntl: function(fd: number, cmd: number, lock_type: number, whence?: number, start?: number, len?: number): number

  -- file flags
  O_RDONLY: number
  O_WRONLY: number
  O_RDWR: number
  O_CREAT: number
  O_EXCL: number
  O_TRUNC: number
  O_APPEND: number

  -- file type tests
  S_ISDIR: function(mode: number): boolean
  S_ISREG: function(mode: number): boolean
  S_ISLNK: function(mode: number): boolean

  -- process status
  WIFEXITED: function(status: number): boolean
  WEXITSTATUS: function(status: number): number

  -- access modes
  F_OK: number
  X_OK: number
  R_OK: number
  W_OK: number

  -- lock modes
  F_SETLK: number
  F_WRLCK: number

  -- signals
  SIGINT: integer
  SIGTERM: integer

  -- network constants
  AF_UNIX: number
  SOCK_STREAM: number

  -- stat flags
  AT_SYMLINK_NOFOLLOW: number

  -- seek whence
  SEEK_SET: number
  SEEK_CUR: number
  SEEK_END: number
end

return unix
