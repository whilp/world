local cosmo = require("cosmo")
local unix = cosmo.unix

local function helper_function(arg)
  if not arg or arg == "" then
    return nil, "arg cannot be empty"
  end
  return arg
end

local function cmd_help(args)
  io.write("mymodule - description\n")
  io.write("\n")
  io.write("usage: mymodule [command] [args]\n")
  io.write("\n")
  io.write("commands:\n")
  io.write("  help    show this help\n")
  io.write("  run     run the module\n")
  return 0
end

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

local function cmd_unknown(command)
  io.stderr:write("unknown command: " .. command .. "\n")
  io.stderr:write("run 'mymodule help' for usage\n")
  return 1
end

local commands = {
  help = cmd_help,
  run = cmd_run,
}

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

return {
  main = main,
  helper_function = helper_function,
}
