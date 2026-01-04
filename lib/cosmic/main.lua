-- cosmic-lua dispatcher
-- entry point for cosmic binary that handles special args and dispatches to features

local getopt = require("cosmo.getopt")

-- Parse arguments using cosmo.getopt
local function parse_args()
  local opts = {
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

  -- Pre-scan for --help and --skill which need special handling
  for i = 1, #arg do
    if arg[i] == "--help" then
      if i + 1 <= #arg and not arg[i + 1]:match("^%-") then
        opts.help = arg[i + 1]
      else
        opts.help = true
      end
      return opts
    elseif arg[i] == "--skill" then
      if i + 1 <= #arg then
        opts.skill = arg[i + 1]
        for j = i + 2, #arg do
          opts.script_args[#opts.script_args + 1] = arg[j]
        end
      end
      return opts
    end
  end

  -- Use getopt for standard options
  local parsed, rest = getopt.parse(arg, "e:l:ivEW")

  -- Collect all -e and -l options (getopt only returns last one)
  local i = 1
  while i <= #arg do
    if arg[i] == "-e" and i + 1 <= #arg then
      opts.execute[#opts.execute + 1] = arg[i + 1]
      i = i + 2
    elseif arg[i] == "-l" and i + 1 <= #arg then
      opts.load[#opts.load + 1] = arg[i + 1]
      i = i + 2
    elseif arg[i] == "--" then
      break
    elseif not arg[i]:match("^%-") then
      break
    else
      i = i + 1
    end
  end

  -- Get other flags from getopt
  opts.interactive = parsed.i or false
  opts.version = parsed.v or false
  opts.warnings = parsed.W or false

  -- Handle remaining arguments (script and script args)
  if rest and #rest > 0 then
    opts.script = rest[1]
    for j = 1, #rest do
      opts.script_args[j - 1] = rest[j]
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
    local ok, mod = pcall(require, opts.help)
    if ok and type(mod) == "table" then
      io.write("Module: " .. opts.help .. "\n")
      if mod._VERSION then
        io.write("Version: " .. mod._VERSION .. "\n")
      end
      if mod._DESCRIPTION then
        io.write("\n" .. mod._DESCRIPTION .. "\n")
      end
      if mod._USAGE then
        io.write("\nUsage:\n" .. mod._USAGE .. "\n")
      end
      os.exit(0)
    else
      io.stderr:write("error: module '" .. opts.help .. "' not found\n")
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

-- Handle --skill
if opts.skill then
  _G.arg = opts.script_args
  local skill = require("skill." .. opts.skill)
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
  local old_warn = warn
  warn = function(...)
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
  _G.arg = opts.script_args
  dofile(opts.script)
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
