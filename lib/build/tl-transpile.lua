#!/usr/bin/env lua
-- Transpile Teal to Lua without type checking
-- Uses tl.parse + tl.generate to bypass type checker
-- Preserves shebang if present

local tl = require("tl")

local function transpile(input_path)
  local f = io.open(input_path, "r")
  if not f then
    io.stderr:write("error: cannot open " .. input_path .. "\n")
    os.exit(1)
  end
  local content = f:read("*a")
  f:close()

  -- Check for shebang
  local shebang = nil
  if content:sub(1, 2) == "#!" then
    local newline = content:find("\n")
    if newline then
      shebang = content:sub(1, newline)
      content = content:sub(newline + 1)
    end
  end

  -- Parse the Teal code (no type checking)
  local ast, errors = tl.parse(content, input_path)
  if not ast then
    io.stderr:write("error: syntax error parsing " .. input_path .. "\n")
    for _, err in ipairs(errors or {}) do
      io.stderr:write("  " .. tostring(err) .. "\n")
    end
    os.exit(1)
  end

  -- Generate Lua from AST (skips type checking)
  local lua_code, gen_err = tl.generate(ast, "lua54")
  if not lua_code then
    io.stderr:write("error: generation failed for " .. input_path .. "\n")
    if gen_err then
      io.stderr:write("  " .. tostring(gen_err) .. "\n")
    end
    os.exit(1)
  end

  -- Output with shebang if present
  if shebang then
    io.write(shebang)
  end
  io.write(lua_code)
end

if #arg < 1 then
  io.stderr:write("usage: tl-transpile.lua <file.tl>\n")
  os.exit(1)
end

transpile(arg[1])
