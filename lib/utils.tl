-- Common utility functions

-- Flatten a nested table into a single-level array
local function flatten(tbl: {any}): {any}
  local result: {any} = {}
  for _, v in ipairs(tbl) do
    if type(v) == "table" then
      for _, inner_v in ipairs(flatten(v as {any})) do
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
local function kv_map<K, V, R>(fn: function({K | V}): R, tbl: {K:V}): {R}
  local result: {R} = {}
  for k, v in pairs(tbl) do
    table.insert(result, fn({k, v}))
  end
  return result
end

-- Join array elements with a separator
local function join(tbl: {string}, sep: string): string
  if not sep then
    return table.concat(tbl)
  end
  return table.concat(tbl, sep)
end

-- Check if a file exists
local function file_exists(path: string): boolean
  local f = io.open(path, "r")
  if f then
    f:close()
    return true
  end
  return false
end

-- Read all lines from a file into an array
local function readlines(path: string): {string}
  local lines: {string} = {}
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
local function expand_path(path: string): string
  if path:sub(1, 1) == "~" then
    local home = os.getenv("HOME") or os.getenv("USERPROFILE")
    if home then
      return home .. path:sub(2)
    end
  end
  return path
end

-- Merge multiple tables together
-- behavior: "force" - later values overwrite earlier ones
--           "keep" - earlier values are kept, later ones ignored for existing keys
local function tbl_extend(behavior: string, ...: {string:any}): {string:any}
  local result: {string:any} = {}
  local tables: {{string:any}} = {...}

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
local function is_windows(): boolean
  return package.config:sub(1, 1) == "\\"
end

return {
  flatten = flatten,
  kv_map = kv_map,
  join = join,
  file_exists = file_exists,
  readlines = readlines,
  expand_path = expand_path,
  tbl_extend = tbl_extend,
  is_windows = is_windows,
}
