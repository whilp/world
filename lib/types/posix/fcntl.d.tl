-- type declarations for posix.fcntl

local record FLock
  l_type: number
  l_whence: number
  l_start: number
  l_len: number
end

local record fcntl
  -- open flags
  O_CREAT: number
  O_RDWR: number
  O_RDONLY: number
  O_WRONLY: number
  O_APPEND: number
  O_TRUNC: number

  -- fcntl commands
  F_SETLK: number
  F_SETLKW: number
  F_GETLK: number

  -- lock types
  F_RDLCK: number
  F_WRLCK: number
  F_UNLCK: number

  -- seek whence
  SEEK_SET: number
  SEEK_CUR: number
  SEEK_END: number

  -- functions
  open: function(path: string, flags: number, mode?: number): number, string
  fcntl: function(fd: number, cmd: number, lock?: FLock): boolean, string
end

return fcntl
