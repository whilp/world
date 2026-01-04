--[[
Environment variable helpers for working with unix.environ() arrays.

unix.environ() returns an array of "KEY=VALUE" strings, not a table.
These helpers provide convenient get/set operations on that format.

Example:
  local env = require("cosmic.env")
  local arr = unix.environ()

  env.set(arr, "CC", "clang")
  local path = env.get(arr, "PATH")
  env.prepend_path(arr, "/usr/local/bin")
]]

local M = {}

-- Get value of environment variable from array
-- Returns nil if not found
function M.get(env, key)
  local prefix = key .. "="
  for _, entry in ipairs(env) do
    if entry:sub(1, #prefix) == prefix then
      return entry:sub(#prefix + 1)
    end
  end
  return nil
end

-- Set environment variable in array
-- Updates existing entry or appends new one
function M.set(env, key, value)
  local prefix = key .. "="
  for i, entry in ipairs(env) do
    if entry:sub(1, #prefix) == prefix then
      env[i] = prefix .. value
      return
    end
  end
  table.insert(env, prefix .. value)
end

-- Remove environment variable from array
-- Returns true if found and removed, false otherwise
function M.unset(env, key)
  local prefix = key .. "="
  for i = #env, 1, -1 do
    if env[i]:sub(1, #prefix) == prefix then
      table.remove(env, i)
      return true
    end
  end
  return false
end

-- Prepend directory to PATH environment variable
function M.prepend_path(env, dir)
  local current = M.get(env, "PATH") or ""
  if current == "" then
    M.set(env, "PATH", dir)
  else
    M.set(env, "PATH", dir .. ":" .. current)
  end
end

-- Append directory to PATH environment variable
function M.append_path(env, dir)
  local current = M.get(env, "PATH") or ""
  if current == "" then
    M.set(env, "PATH", dir)
  else
    M.set(env, "PATH", current .. ":" .. dir)
  end
end

return M
