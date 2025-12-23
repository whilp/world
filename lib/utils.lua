--[[
Common utility functions for Lua

General purpose utilities that can be used across different modules.
]]

local M = {}

-- Flatten a nested table into a single-level array
-- Example: flatten({{1, 2}, {3, {4, 5}}}) -> {1, 2, 3, 4, 5}
M.flatten = function(tbl)
  local result = {}
  for _, v in ipairs(tbl) do
    if type(v) == "table" then
      for _, inner_v in ipairs(M.flatten(v)) do
        table.insert(result, inner_v)
      end
    else
      table.insert(result, v)
    end
  end
  return result
end

-- Map a function over key-value pairs in a table
-- The function receives {key, value} pairs
-- Example: kv_map(function(kv) return kv[1] .. "=" .. kv[2] end, {a=1, b=2})
M.kv_map = function(fn, tbl)
  local result = {}
  for k, v in pairs(tbl) do
    table.insert(result, fn({k, v}))
  end
  return result
end

-- Join array elements with a separator
-- Example: join({"a", "b", "c"}, ",") -> "a,b,c"
M.join = function(tbl, sep)
  if not sep then
    return table.concat(tbl)
  end
  return table.concat(tbl, sep)
end

-- Check if a file exists
M.file_exists = function(path)
  local f = io.open(path, "r")
  if f then
    f:close()
    return true
  end
  return false
end

-- Read all lines from a file into an array
M.readlines = function(path)
  local lines = {}
  local file = io.open(path, "r")
  if not file then
    return lines
  end
  for line in file:lines() do
    table.insert(lines, line)
  end
  file:close()
  return lines
end

-- Expand ~ in paths to home directory
M.expand_path = function(path)
  if path:sub(1, 1) == "~" then
    local home = os.getenv("HOME") or os.getenv("USERPROFILE")
    return home .. path:sub(2)
  end
  return path
end

-- Merge multiple tables together
-- behavior: "force" - later values overwrite earlier ones
--           "keep" - earlier values are kept, later ones ignored for existing keys
M.tbl_extend = function(behavior, ...)
  local result = {}
  local tables = {...}

  if behavior == "force" then
    for _, t in ipairs(tables) do
      for k, v in pairs(t) do
        result[k] = v
      end
    end
  elseif behavior == "keep" then
    for _, t in ipairs(tables) do
      for k, v in pairs(t) do
        if result[k] == nil then
          result[k] = v
        end
      end
    end
  end

  return result
end

-- Check if running on Windows
M.is_windows = function()
  return package.config:sub(1, 1) == "\\"
end

return M
