local cosmo = require("cosmo")
local unix = cosmo.unix

-- Helper functions (local, not exported)
local function helper_function(arg)
  if not arg or arg == "" then
    return nil, "arg cannot be empty"
  end

  -- implementation
  return true
end

-- Command: help
local function cmd_help(args)
  io.write("mymodule - description\n")
  io.write("\n")
  io.write("usage: mymodule [command] [args]\n")
  io.write("\n")
  io.write("commands:\n")
  io.write("  help    show this help\n")
  io.write("  run     run the module\n")
  io.write("  env     example with environment variables\n")
  return 0
end

-- Command: run
local function cmd_run(args)
  if #args == 0 then
    io.stderr:write("usage: mymodule run <arg>\n")
    return 1
  end

  local result, err = helper_function(args[1])
  if not result then
    io.stderr:write("error: " .. err .. "\n")
    return 1
  end

  io.write("success\n")
  return 0
end

-- Command: env (example with environment variables)
local function cmd_env(args)
  -- Read specific environment variable
  local home = os.getenv("HOME")
  if home then
    io.write("HOME=" .. home .. "\n")
  end

  -- Build custom environment for subprocess
  local env = unix.environ()
  env["CUSTOM_VAR"] = "custom_value"

  -- Example: could use env with unix.execve()
  -- unix.execve("/usr/bin/env", {"env"}, env)

  io.write("environment prepared\n")
  return 0
end

-- Command: unknown
local function cmd_unknown(command)
  io.stderr:write("unknown command: " .. command .. "\n")
  io.stderr:write("run 'mymodule help' for usage\n")
  return 1
end

-- Command dispatch table
local commands = {
  help = cmd_help,
  run = cmd_run,
  env = cmd_env,
}

-- Main entry point
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

-- Export functions for testing
return {
  main = main,
  helper_function = helper_function,
}
