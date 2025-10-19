-- json_pp.lua: JSON pretty printer compatible with shimlink config format
-- Uses 2-space indentation and sorts object keys alphabetically

local json = require("json")

local M = {}

-- Escape string for JSON
local function escape_string(str)
  local escape_map = {
    ["\\"] = "\\\\",
    ['"'] = '\\"',
    ["\b"] = "\\b",
    ["\f"] = "\\f",
    ["\n"] = "\\n",
    ["\r"] = "\\r",
    ["\t"] = "\\t",
  }
  return (str:gsub('[\\"\b\f\n\r\t]', escape_map))
end

-- Check if table is an array (sequential numeric keys starting from 1)
local function is_array(t)
  if type(t) ~= "table" then
    return false
  end
  local i = 0
  for _ in pairs(t) do
    i = i + 1
    if t[i] == nil then
      return false
    end
  end
  return true
end

-- Get sorted keys for objects
local function get_sorted_keys(t)
  local keys = {}
  for k in pairs(t) do
    table.insert(keys, k)
  end
  table.sort(keys)
  return keys
end

-- Pretty print with indentation
local function pp_value(value, indent, current_indent)
  local indent_str = string.rep(" ", current_indent)
  local next_indent = current_indent + indent
  local next_indent_str = string.rep(" ", next_indent)

  if value == nil then
    return "null"
  elseif type(value) == "boolean" then
    return value and "true" or "false"
  elseif type(value) == "number" then
    return tostring(value)
  elseif type(value) == "string" then
    return '"' .. escape_string(value) .. '"'
  elseif type(value) == "table" then
    if next(value) == nil then
      -- Empty table
      if is_array(value) then
        return "[]"
      else
        return "{}"
      end
    elseif is_array(value) then
      -- Array
      local parts = {}
      for i, v in ipairs(value) do
        table.insert(parts, next_indent_str .. pp_value(v, indent, next_indent))
      end
      return "[\n" .. table.concat(parts, ",\n") .. "\n" .. indent_str .. "]"
    else
      -- Object
      local parts = {}
      local keys = get_sorted_keys(value)
      for _, k in ipairs(keys) do
        local v = value[k]
        local key_str = '"' .. escape_string(k) .. '"'
        local value_str = pp_value(v, indent, next_indent)
        table.insert(parts, next_indent_str .. key_str .. ": " .. value_str)
      end
      return "{\n" .. table.concat(parts, ",\n") .. "\n" .. indent_str .. "}"
    end
  else
    error("unsupported type: " .. type(value))
  end
end

-- Pretty print JSON with 2-space indentation (compatible with shimlink config)
function M.encode(value)
  return pp_value(value, 2, 0)
end

-- Parse and pretty print JSON string
function M.prettify(json_str)
  local value = json.decode(json_str)
  return M.encode(value)
end

return M
