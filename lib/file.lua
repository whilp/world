local unix = require("cosmo.unix")
local path = require("cosmo.path")
local stat = require("posix.sys.stat")
local dirent = require("posix.dirent")
local unistd = require("posix.unistd")

local M = {}

function M.exists(file_path)
  local st = stat.stat(file_path)
  return st ~= nil
end

function M.is_directory(file_path)
  local st, err = stat.stat(file_path)
  if not st then
    return nil, "failed to stat path: " .. tostring(err)
  end
  return stat.S_ISDIR(st.st_mode) ~= 0
end

function M.read(file_path)
  local f, err = io.open(file_path, "rb")
  if not f then
    return nil, "failed to open file: " .. tostring(err)
  end
  local content = f:read("*all")
  f:close()
  return content
end

function M.write(file_path, content)
  local f, err = io.open(file_path, "wb")
  if not f then
    return nil, "failed to open file for writing: " .. tostring(err)
  end
  f:write(content)
  f:close()
  return true
end

function M.basename(file_path)
  return file_path:match("([^/]+)$") or file_path
end

function M.dirname(file_path)
  local dir = file_path:match("(.+)/[^/]+$")
  return dir or "."
end

function M.path_join(...)
  local parts = { ... }
  return table.concat(parts, "/")
end

function M.expand_path(file_path)
  if file_path:sub(1, 1) == "~" then
    local home = os.getenv("HOME") or os.getenv("USERPROFILE")
    return path.join(home, file_path:sub(2))
  end
  return file_path
end

function M.mkdir_p(dir_path)
  local ok, err = unix.makedirs(dir_path, tonumber("0755", 8))
  if not ok then
    error("failed to create directory " .. dir_path .. ": " .. tostring(err))
  end
end

function M.rm_rf(target_path)
  local is_dir = M.is_directory(target_path)
  if not M.exists(target_path) and not is_dir then
    return true
  end

  local function remove_recursive(p)
    local p_is_dir = M.is_directory(p)
    if p_is_dir then
      local entries, err = dirent.dir(p)
      if not entries then
        return nil, "failed to read directory during removal: " .. tostring(err)
      end
      for _, entry in ipairs(entries) do
        if entry ~= "." and entry ~= ".." then
          local ok, rec_err = remove_recursive(M.path_join(p, entry))
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

function M.list_dir_first_entry(dir_path)
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

return M
