-- cosmic.lfs: LuaFileSystem compatibility stub
local unix = require("cosmo.unix")

local record Attrs
  mode: string
  size: number
  modification: number
end

local record lfs
  _VERSION: string
  attributes: function(filepath: string, aname?: string): Attrs | string | number, string
  currentdir: function(): string
  dir: function(path: string): function(): string
  mkdir: function(dirname: string): boolean, string
end

local M: lfs = {
  _VERSION = "LuaFileSystem 1.8.0 (stub)",
}

function M.attributes(filepath: string, aname?: string): Attrs | string | number, string
  local st = unix.stat(filepath)
  if not st then
    return nil, "cannot obtain information from file `" .. filepath .. "'"
  end

  local mode = st:mode()
  local filetype: string
  if (mode & 0xF000) == 0x4000 then
    filetype = "directory"
  elseif (mode & 0xF000) == 0x8000 then
    filetype = "file"
  else
    filetype = "other"
  end

  local attrs: Attrs = {
    mode = filetype,
    size = st:size(),
    modification = st:mtim(),
  }

  if aname then
    return (attrs as {string:string | number})[aname]
  end
  return attrs
end

function M.currentdir(): string
  return unix.getcwd()
end

function M.dir(path: string): function(): string
  local dir, err = unix.opendir(path)
  if not dir then
    error("cannot open " .. path .. ": " .. (err or "unknown error"))
  end

  return function(): string
    return dir:read()
  end
end

function M.mkdir(dirname: string): boolean, string
  local ok, err = unix.mkdir(dirname, 448)
  if not ok then
    return nil, err
  end
  return true
end

return M
