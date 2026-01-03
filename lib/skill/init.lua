-- skill - dispatcher for cosmic skill commands
-- usage: cosmic-lua -l skill <subcommand> [args...]

-- teal ignore: type annotations needed
local cosmo = require("cosmo")

local function find_subcommand()
  if not arg then return nil end

  for i = -1, #arg do
    if arg[i] == "update-pr" then
      return "update-pr", i
    end
  end

  return nil
end

-- Only dispatch if we're being invoked (have a subcommand in args)
-- This allows the module to be safely required without side effects
local subcommand, cmd_idx = find_subcommand()

if subcommand then
  -- rebuild arg table with subcommand args only
  local new_arg = {}
  if cmd_idx then
    for i = cmd_idx + 1, #arg do
      new_arg[#new_arg + 1] = arg[i]
    end
  end

  _G.arg = new_arg

  if subcommand == "update-pr" then
    local pr = require("skill.pr")
    local code, msg = pr.main()
    if msg then
      io.stderr:write(msg .. "\n")
    end
    os.exit(code or 0)
  else
    io.stderr:write("skill: unknown subcommand: " .. subcommand .. "\n")
    os.exit(1)
  end
end

return {
  _VERSION = "0.1.0",
  _DESCRIPTION = "Skill dispatcher for cosmic-lua",
}
