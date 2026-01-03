-- skill.pr - PR management skill for cosmic-lua
-- usage: cosmic-lua -l skill.pr update

local pr = require("build.pr")

-- when loaded via -l skill.pr, arg table will have script args
-- check if we're being invoked as a skill (not just required)
local function is_skill_invocation()
  -- when using -l, arg[0] is typically the script name or nil
  -- we check the arg table for our subcommands
  if not arg then return false end

  for i = -1, #arg do
    if arg[i] == "update" then
      return true, i
    end
  end

  return false
end

local is_skill, cmd_idx = is_skill_invocation()

if is_skill then
  -- rebuild arg table with subcommand args only
  local new_arg = {}
  if cmd_idx then
    for i = cmd_idx + 1, #arg do
      new_arg[#new_arg + 1] = arg[i]
    end
  end

  -- replace global arg with command-specific args
  _G.arg = new_arg

  -- dispatch to pr.main
  local code, msg = pr.main()
  if msg then
    io.stderr:write(msg .. "\n")
  end
  os.exit(code or 0)
end

-- if not a skill invocation, just return the pr module for use as library
return pr
