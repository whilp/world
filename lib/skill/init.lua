-- skill - dispatcher for cosmic skill commands
-- usage: cosmic --skill <subcommand> [args...]

-- teal ignore: type annotations needed
local cosmo = require("cosmo")

local subcommands = {
  ["update-pr"] = "skill.pr",
  ["pr-comments"] = "skill.pr_comments",
}

local function find_subcommand()
  if not arg then return nil end

  for i = -1, #arg do
    if subcommands[arg[i]] then
      return arg[i], i
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

  local module_name = subcommands[subcommand]
  local mod = require(module_name)
  local code, msg = mod.main(table.unpack(new_arg))
  if msg then
    io.stderr:write(msg .. "\n")
  end
  os.exit(code or 0)
end

return {
  _VERSION = "0.1.0",
  _DESCRIPTION = "Skill dispatcher for cosmic-lua",
}
