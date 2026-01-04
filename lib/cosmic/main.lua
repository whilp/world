-- cosmic-lua dispatcher
-- entry point for cosmic binary that handles special args and dispatches to features

local unix = require("cosmo.unix")

local function find_arg(name)
  if not arg then return nil, nil end
  for i = 1, #arg do
    if arg[i] == name then
      return true, i
    end
  end
  return false, nil
end

local function has_prefix(str, prefix)
  return str and str:sub(1, #prefix) == prefix
end

-- Check if we have lua-specific flags that need lua's native processing
local function has_lua_flags()
  if not arg or #arg == 0 then
    return false
  end
  local lua_flags = {"-e", "-l", "-i", "-v", "-E", "-W"}
  for _, flag in ipairs(lua_flags) do
    if find_arg(flag) then
      return true
    end
  end
  -- Also check if first arg is a script file (doesn't start with -)
  if arg[1] and not has_prefix(arg[1], "-") then
    return true
  end
  return false
end

-- If we have lua-specific flags, process them ourselves since .args prevents lua from handling them
if has_lua_flags() then
  -- Process lua flags manually
  local i = 1
  while i <= #arg do
    local flag = arg[i]

    if flag == "-e" then
      -- Execute string
      i = i + 1
      if i <= #arg then
        local chunk, err = load(arg[i], "=(command line)")
        if chunk then
          chunk()
        else
          io.stderr:write("cosmic-lua: " .. (err or "error loading command") .. "\n")
          os.exit(1)
        end
      end
    elseif flag == "-l" then
      -- Load library
      i = i + 1
      if i <= #arg then
        require(arg[i])
      end
    elseif flag == "-i" then
      -- Interactive mode (TODO: implement REPL)
      io.stderr:write("cosmic-lua: interactive mode not yet implemented\n")
      os.exit(1)
    elseif flag == "-v" then
      -- Version
      io.write(_VERSION .. "\n")
      os.exit(0)
    elseif flag == "-E" then
      -- Ignore environment variables (already handled by lua)
      -- Skip
    elseif flag == "-W" then
      -- Turn warnings into errors (TODO: implement)
      -- Skip for now
    elseif flag == "--" then
      -- End of options
      break
    elseif not has_prefix(flag, "-") then
      -- Script file
      local script_arg = {}
      for j = i, #arg do
        script_arg[j - i] = arg[j]
      end
      script_arg[-1] = arg[-1]
      _G.arg = script_arg
      dofile(flag)
      os.exit(0)
    end

    i = i + 1
  end

  -- If we processed flags but didn't exit, just exit normally
  os.exit(0)
end

-- Check for --skill arg
local has_skill, skill_idx = find_arg("--skill")
if has_skill and arg[skill_idx + 1] then
  -- rebuild arg table for skill dispatcher
  local skill_name = arg[skill_idx + 1]
  local new_arg = {skill_name}
  for i = skill_idx + 2, #arg do
    new_arg[#new_arg + 1] = arg[i]
  end
  _G.arg = new_arg

  -- load and dispatch to skill
  local skill = require("skill." .. skill_name)
  if skill.main then
    local code, msg = skill.main()
    if msg then
      io.stderr:write(msg .. "\n")
    end
    os.exit(code or 0)
  else
    io.stderr:write("error: skill '" .. skill_name .. "' has no main function\n")
    os.exit(1)
  end
end

-- Check for --help with optional module arg
local has_help, help_idx = find_arg("--help")
if has_help then
  local module_name = arg[help_idx + 1]

  if module_name and not has_prefix(module_name, "-") then
    -- help for specific module
    local ok, mod = pcall(require, module_name)
    if ok and type(mod) == "table" then
      io.write("Module: " .. module_name .. "\n")
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
      io.stderr:write("error: module '" .. module_name .. "' not found\n")
      os.exit(1)
    end
  else
    -- general cosmic help - wrap lua's help with cosmic info
    io.write("cosmic-lua: cosmopolitan lua with bundled libraries\n")
    io.write("\n")
    io.write("Usage: cosmic-lua [options] [script [args]]\n")
    io.write("\n")
    io.write("Cosmic options:\n")
    io.write("  --skill <name> [args]    run a skill module\n")
    io.write("  --help [module]          show help for cosmic or a module\n")
    io.write("\n")
    io.write("Standard lua options:\n")
    io.write("  (use 'cosmic-lua' without args for lua interactive mode)\n")
    os.exit(0)
  end
end

-- No special args - let lua proceed normally
-- The lua interpreter will handle standard args like -e, -l, etc.
