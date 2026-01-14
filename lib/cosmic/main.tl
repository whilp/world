-- cosmic-lua dispatcher
-- entry point for cosmic binary that handles special args and dispatches to features

require("tl").loader()

local getopt = require("cosmo.getopt")

-- Long option definition: {name, arg_requirement, short_alias}
local type LongOpt = {string, string, string}

-- Parsed options record
local record Opts
  execute: {string}
  load: {string}
  interactive: boolean
  version: boolean
  warnings: boolean
  skill: string
  help: boolean | string
  script: string
  script_args: {integer:string}
end

-- Parse arguments using cosmo.getopt iterator API
local function parse_args(): Opts
  local opts: Opts = {
    execute = {},
    load = {},
    interactive = false,
    version = false,
    warnings = false,
    skill = nil,
    help = nil,
    script = nil,
    script_args = {},
  }

  local shortopts = "e:l:ivEWh::s:"
  local longopts: {LongOpt} = {
    { "help", "optional", "h" },
    { "skill", "required", "s" },
  }

  -- Parse shortopts to build set of options that require arguments
  local short_needs_arg: {string:boolean} = {}
  local j = 1
  while j <= #shortopts do
    local c = shortopts:sub(j, j)
    if c ~= ":" then
      -- Check if followed by : (required arg) but not :: (optional arg)
      if j < #shortopts and shortopts:sub(j+1, j+1) == ":" and
         (j+1 >= #shortopts or shortopts:sub(j+2, j+2) ~= ":") then
        short_needs_arg[c] = true
      end
    end
    j = j + 1
  end

  -- Parse longopts to build set of options that require arguments
  local long_needs_arg: {string:boolean} = {}
  for _, opt in ipairs(longopts) do
    if opt[2] == "required" then
      long_needs_arg[opt[1]] = true
    end
  end

  -- Find first non-option argument (script name) to know where to stop parsing
  local script_idx: integer = nil
  local i = 1
  while i <= #arg do
    local a = arg[i]
    if a == "--" then
      -- Explicit end of options
      script_idx = i + 1
      break
    elseif a:sub(1, 2) == "--" then
      -- Long option
      local opt_name = a:sub(3)
      if not opt_name:find("=") and long_needs_arg[opt_name] then
        -- Takes next arg
        i = i + 2
      else
        i = i + 1
      end
    elseif a:sub(1, 1) == "-" and #a > 1 then
      -- Short option
      if #a == 2 and short_needs_arg[a:sub(2, 2)] then
        -- Single short option that takes arg
        i = i + 2
      else
        i = i + 1
      end
    else
      -- First non-option is the script name
      script_idx = i
      break
    end
  end

  -- Only parse cosmic options (before script name)
  local cosmic_args: {string} = {}
  if script_idx then
    for i = 1, script_idx - 1 do
      if arg[i] ~= "--" then  -- Exclude -- separator
        cosmic_args[#cosmic_args + 1] = arg[i]
      end
    end
  else
    cosmic_args = arg
  end

  local parser = getopt.new(cosmic_args, shortopts, longopts)

  -- Iterate through all options
  while true do
    local opt, optarg = parser:next()
    if not opt then
      break
    end

    if opt == "?" then
      -- Unknown cosmic option
      io.stderr:write("cosmic-lua: unknown option '" .. optarg .. "'\n")
      os.exit(1)
    elseif opt == "e" then
      opts.execute[#opts.execute + 1] = optarg
    elseif opt == "l" then
      opts.load[#opts.load + 1] = optarg
    elseif opt == "i" then
      opts.interactive = true
    elseif opt == "v" then
      opts.version = true
    elseif opt == "E" then
      -- Ignore environment variables (already handled by lua)
    elseif opt == "W" then
      opts.warnings = true
    elseif opt == "h" or opt == "help" then
      if optarg then
        opts.help = optarg
      else
        opts.help = true
      end
      return opts
    elseif opt == "s" or opt == "skill" then
      opts.skill = optarg
      -- Remaining args go to skill
      opts.script_args = parser:remaining() as {integer:string}
      return opts
    end
  end

  -- Handle script and script args (from original arg, starting at script_idx)
  if script_idx then
    opts.script = arg[script_idx]
    opts.script_args[0] = arg[script_idx]
    local arg_num = 1
    for i = script_idx + 1, #arg do
      -- Skip -- separator
      if arg[i] ~= "--" then
        opts.script_args[arg_num] = arg[i]
        arg_num = arg_num + 1
      end
    end
    opts.script_args[-1] = arg[-1]
  end

  return opts
end

-- Simple REPL using debug.debug
local function run_repl()
  io.write(_VERSION .. "  Copyright (C) 1994-2024 Lua.org, PUC-Rio\n")
  debug.debug()
end

local opts = parse_args()

-- Handle -v
if opts.version then
  io.write(_VERSION .. "\n")
  os.exit(0)
end

-- Handle --help
if opts.help then
  if type(opts.help) == "string" then
    -- Help for specific module
    local ok, mod = pcall(require, opts.help as string)
    if ok and type(mod) == "table" then
      io.write("Module: " .. (opts.help as string) .. "\n")
      local modtbl = mod as {string:string}
      if modtbl._VERSION then
        io.write("Version: " .. modtbl._VERSION .. "\n")
      end
      if modtbl._DESCRIPTION then
        io.write("\n" .. modtbl._DESCRIPTION .. "\n")
      end
      if modtbl._USAGE then
        io.write("\nUsage:\n" .. modtbl._USAGE .. "\n")
      end
      os.exit(0)
    else
      io.stderr:write("error: module '" .. (opts.help as string) .. "' not found\n")
      os.exit(1)
    end
  else
    -- General cosmic help
    io.write("cosmic-lua: cosmopolitan lua with bundled libraries\n")
    io.write("\n")
    io.write("Usage: cosmic-lua [options] [script [args]]\n")
    io.write("\n")
    io.write("Cosmic options:\n")
    io.write("  --skill <name> [args]    run a skill module\n")
    io.write("  --help [module]          show help for cosmic or a module\n")
    io.write("\n")
    io.write("Standard lua options:\n")
    io.write("  -e <stat>                execute string 'stat'\n")
    io.write("  -l <name>                require library 'name'\n")
    io.write("  -i                       enter interactive mode\n")
    io.write("  -v                       show version information\n")
    io.write("  -E                       ignore environment variables\n")
    io.write("  -W                       turn warnings into errors\n")
    os.exit(0)
  end
end

-- Skill module interface
local record SkillModule
  main: function(): integer, string
end

-- Handle --skill
if opts.skill then
  _G.arg = opts.script_args as {string}
  local skill = require("skill." .. opts.skill) as SkillModule
  if skill.main then
    local code, msg = skill.main()
    if msg then
      io.stderr:write(msg .. "\n")
    end
    os.exit(code or 0)
  else
    io.stderr:write("error: skill '" .. opts.skill .. "' has no main function\n")
    os.exit(1)
  end
end

-- Handle -W warnings: convert warnings to errors
if opts.warnings then
  warn = function(...: string)
    local msg = table.concat({...}, " ")
    error("warning: " .. msg, 2)
  end
end

-- Load libraries
for _, name in ipairs(opts.load) do
  require(name)
end

-- Execute strings
for _, code in ipairs(opts.execute) do
  local chunk, err = load(code, "=(command line)")
  if chunk then
    chunk()
  else
    io.stderr:write("cosmic-lua: " .. (err or "error loading command") .. "\n")
    os.exit(1)
  end
end

-- Execute script file
if opts.script then
  _G.arg = opts.script_args as {string}
  local chunk, err = loadfile(opts.script)
  if not chunk then
    io.stderr:write("cosmic-lua: " .. err .. "\n")
    os.exit(1)
  end
  -- Call chunk with script args as varargs (matching standard lua behavior)
  -- Standard lua passes arg[1], arg[2], ... as varargs to the script
  -- Note: table.unpack has a limit of ~250 args, but this is not a practical concern
  local args: {string} = {}
  for i = 1, #opts.script_args do
    args[i] = opts.script_args[i]
  end
  chunk(table.unpack(args))
  os.exit(0)
end

-- Interactive mode or REPL
if opts.interactive or (#opts.execute == 0 and #opts.load == 0 and #arg == 0) then
  run_repl()
  os.exit(0)
end

-- If we have -e or -l but no script, exit normally
if #opts.execute > 0 or #opts.load > 0 then
  os.exit(0)
end
