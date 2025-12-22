--[[
Environment variable table wrapper

Provides a table-like interface for environment variables that are typically
represented as an array of "KEY=VALUE" strings (like unix.environ() returns).

Example:
  local environ = require("environ")
  local env = environ.new(unix.environ())

  env.GH_HOST = "github.com"
  local host = env.GH_HOST

  unix.execve(path, args, env:toarray())
]]

local M = {}

local Environ = {}
Environ.__index = function(self, key)
  if Environ[key] then
    return Environ[key]
  end
  return rawget(self, "_vars")[key]
end

Environ.__newindex = function(self, key, value)
  rawget(self, "_vars")[key] = value
end

function Environ:toarray()
  local result = {}
  for k, v in pairs(self._vars) do
    table.insert(result, k .. "=" .. v)
  end
  return result
end

function M.new(arr)
  local vars = {}

  if arr then
    for _, entry in ipairs(arr) do
      local eq_pos = entry:find("=")
      if eq_pos then
        local key = entry:sub(1, eq_pos - 1)
        local value = entry:sub(eq_pos + 1)
        vars[key] = value
      end
    end
  end

  local obj = {_vars = vars}
  setmetatable(obj, Environ)
  return obj
end

return M
