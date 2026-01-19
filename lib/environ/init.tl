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

local record Environ
  _vars: {string:string}
  toarray: function(Environ): {string}
  metamethod __index: function(Environ, string): string
  metamethod __newindex: function(Environ, string, string)
end

local Environ_mt: metatable<Environ> = {}

Environ_mt.__index = function(self: Environ, key: string): any
  if key == "toarray" then
    return function(s: Environ): {string}
      local result: {string} = {}
      for k, v in pairs(s._vars) do
        table.insert(result, k .. "=" .. v)
      end
      return result
    end
  end
  return rawget(self, "_vars")[key]
end

Environ_mt.__newindex = function(self: Environ, key: string, value: string)
  rawget(self, "_vars")[key] = value
end

local function new(arr?: {string}): Environ
  local vars: {string:string} = {}

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

  local obj: Environ = {_vars = vars}
  setmetatable(obj, Environ_mt)
  return obj
end

return {
  new = new,
}
