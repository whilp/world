-- type declarations for posix.sys.stat

local record StatInfo
  st_mode: number
  st_size: number
  st_mtime: number
  st_atime: number
  st_ctime: number
  st_uid: number
  st_gid: number
  st_nlink: number
  st_ino: number
  st_dev: number
end

local record stat
  stat: function(path: string): StatInfo, string
  lstat: function(path: string): StatInfo, string
  S_ISDIR: function(mode: number): number
  S_ISREG: function(mode: number): number
  S_ISLNK: function(mode: number): number
  S_ISBLK: function(mode: number): number
  S_ISCHR: function(mode: number): number
  S_ISFIFO: function(mode: number): number
  S_ISSOCK: function(mode: number): number
end

return stat
