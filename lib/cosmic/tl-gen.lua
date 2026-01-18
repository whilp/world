-- teal ignore: bootstrap script
-- tl gen implementation using bundled tl.lua
-- Usage: cosmic -- tl-gen.lua INPUT -o OUTPUT

local tl = require("tl")
local getopt = require("cosmo.getopt")

local function main(...)
  local args = {...}

  local longopts = {
    {"output", "required", "o"},
  }
  local parser = getopt.new(args, "o:", longopts)

  local input_file = nil
  local output_file = nil

  while true do
    local opt, optarg = parser:next()
    if not opt then break end

    if opt == "?" then
      io.stderr:write("error: invalid option\n")
      return 1
    elseif opt == "o" or opt == "output" then
      output_file = optarg
    end
  end

  local remaining = parser:remaining()
  if remaining and #remaining > 0 then
    input_file = remaining[1]
  end

  if not input_file or not output_file then
    io.stderr:write("usage: tl-gen.lua INPUT -o OUTPUT\n")
    return 1
  end

  -- Read input file
  local f, err = io.open(input_file, "r")
  if not f then
    io.stderr:write("error: cannot open " .. input_file .. ": " .. (err or "unknown") .. "\n")
    return 1
  end
  local input = f:read("*a")
  f:close()

  -- Lex the input
  local tokens = tl.lex(input)
  if not tokens then
    io.stderr:write("error: lexing failed for " .. input_file .. "\n")
    return 1
  end

  -- Parse the program
  local ast, errs = tl.parse_program(tokens, {}, input_file)

  -- Only report errors that have actual error messages
  local real_errors = {}
  if errs then
    for _, e in ipairs(errs) do
      if e.msg then
        table.insert(real_errors, e)
      end
    end
  end

  if #real_errors > 0 then
    for _, e in ipairs(real_errors) do
      io.stderr:write(string.format("%s:%d:%d: %s\n", input_file, e.y or 0, e.x or 0, e.msg))
    end
    return 1
  end

  if not ast then
    io.stderr:write("error: parsing failed for " .. input_file .. "\n")
    return 1
  end

  -- Generate lua code (no type checking - just transpile)
  local lua_code = tl.generate(ast, {})

  -- Write output
  local out, write_err = io.open(output_file, "w")
  if not out then
    io.stderr:write("error: cannot write " .. output_file .. ": " .. (write_err or "unknown") .. "\n")
    return 1
  end
  out:write(lua_code)
  out:close()

  return 0
end

-- Support both varargs and arg table (for dofile)
local args = {...}
if #args == 0 and arg then
  args = arg
end
local code = main(table.unpack(args))
os.exit(code)
