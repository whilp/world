local unix = require("cosmo.unix")
local path = require("cosmo.path")
local stat = require("posix.sys.stat")
local dirent = require("posix.dirent")
local unistd = require("posix.unistd")

local function exists(file_path: string): boolean
  local st = stat.stat(file_path)
  return st ~= nil
end

local function is_directory(file_path: string): boolean, string
  local st, err = stat.stat(file_path)
  if not st then
    return nil, "failed to stat path: " .. tostring(err)
  end
  return stat.S_ISDIR(st.st_mode) ~= 0
end

local function read(file_path: string): string, string
  local f, err = io.open(file_path, "rb")
  if not f then
    return nil, "failed to open file: " .. tostring(err)
  end
  local content = f:read("*all")
  f:close()
  return content
end

local function write(file_path: string, content: string): boolean, string
  local f, err = io.open(file_path, "wb")
  if not f then
    return nil, "failed to open file for writing: " .. tostring(err)
  end
  f:write(content)
  f:close()
  return true
end

local function basename(file_path: string): string
  return file_path:match("([^/]+)$") or file_path
end

local function dirname(file_path: string): string
  local dir = file_path:match("(.+)/[^/]+$")
  return dir or "."
end

local function path_join(...: string): string
  local parts: {string} = { ... }
  return table.concat(parts, "/")
end

local function expand_path(file_path: string): string
  if file_path:sub(1, 1) == "~" then
    local home = os.getenv("HOME") or os.getenv("USERPROFILE")
    return path.join(home, file_path:sub(2))
  end
  return file_path
end

local function mkdir_p(dir_path: string)
  local ok, err = unix.makedirs(dir_path, tonumber("0755", 8))
  if not ok then
    error("failed to create directory " .. dir_path .. ": " .. tostring(err))
  end
end

local function rm_rf(target_path: string): boolean, string
  local is_dir = is_directory(target_path)
  if not exists(target_path) and not is_dir then
    return true
  end

  local function remove_recursive(p: string): boolean, string
    local p_is_dir = is_directory(p)
    if p_is_dir then
      local entries, err = dirent.dir(p)
      if not entries then
        return nil, "failed to read directory during removal: " .. tostring(err)
      end
      for _, entry in ipairs(entries) do
        if entry ~= "." and entry ~= ".." then
          local ok, rec_err = remove_recursive(path_join(p, entry))
          if not ok then
            return nil, rec_err
          end
        end
      end
      local result, rmdir_err = unistd.rmdir(p)
      if not result then
        return nil, "failed to remove directory: " .. tostring(rmdir_err)
      end
    else
      local result = unix.unlink(p)
      if not result then
        return nil, "failed to unlink file: " .. p
      end
    end
    return true
  end

  return remove_recursive(target_path)
end

local function list_dir_first_entry(dir_path: string): string, string
  local entries, err = dirent.dir(dir_path)
  if not entries then
    return nil, "failed to read directory: " .. tostring(err)
  end
  for _, entry in ipairs(entries) do
    if entry ~= "." and entry ~= ".." then
      return entry
    end
  end
  return nil, "directory is empty"
end

return {
  exists = exists,
  is_directory = is_directory,
  read = read,
  write = write,
  basename = basename,
  dirname = dirname,
  path_join = path_join,
  expand_path = expand_path,
  mkdir_p = mkdir_p,
  rm_rf = rm_rf,
  list_dir_first_entry = list_dir_first_entry,
}
