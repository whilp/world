local cosmo = require("cosmo")
local unix = cosmo.unix

-- Public function with error handling
local function function_name(arg)
  -- Validate input
  if not arg or arg == "" then
    return nil, "arg cannot be empty"
  end

  -- Implementation
  local result = arg

  -- Return success
  return result
end

-- Another public function
local function another_function(path)
  local stat = unix.stat(path)
  if not stat then
    return nil, "path does not exist: " .. path
  end

  return true
end

-- Command: help
local function cmd_help(args)
  io.write("module - description\n")
  io.write("\n")
  io.write("usage: module [command] [args]\n")
  io.write("\n")
  io.write("commands:\n")
  io.write("  help    show this help\n")
  io.write("  run     run the module\n")
  return 0
end

-- Command: run
local function cmd_run(args)
  if #args == 0 then
    io.stderr:write("usage: module run <arg>\n")
    return 1
  end

  local result, err = function_name(args[1])
  if not result then
    io.stderr:write("error: " .. err .. "\n")
    return 1
  end

  io.write("success\n")
  return 0
end

-- Command: unknown
local function cmd_unknown(command)
  io.stderr:write("unknown command: " .. command .. "\n")
  io.stderr:write("run 'module help' for usage\n")
  return 1
end

-- Command dispatch table
local commands = {
  help = cmd_help,
  run = cmd_run,
}

-- Main entry point (optional, for direct execution)
local function main(args)
  if #args == 0 then
    return cmd_help(args)
  end

  local command = args[1]
  local cmd_args = { unpack(args, 2) }
  local cmd_fn = commands[command]

  if cmd_fn then
    return cmd_fn(cmd_args)
  else
    return cmd_unknown(command)
  end
end

-- Assemble public interface
local M = {
  function_name = function_name,
  another_function = another_function,
  main = main,
}

-- Run main if executed directly (not required as a module)
if not pcall(debug.getlocal, 4, 1) then
  local exit_code = main(arg)
  os.exit(exit_code or 0)
end

return M
