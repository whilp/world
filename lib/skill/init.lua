-- skill - dispatcher for cosmic skill commands
-- usage: cosmic-lua -l skill <subcommand> [args...]

local function is_skill_invocation()
  if not arg then return false end

  for i = -1, #arg do
    if arg[i] == "update-pr" then
      return true, i, "update-pr"
    end
  end

  return false
end

local is_skill, cmd_idx, subcommand = is_skill_invocation()

if is_skill then
  -- rebuild arg table with subcommand args only
  local new_arg = {}
  if cmd_idx then
    for i = cmd_idx + 1, #arg do
      new_arg[#new_arg + 1] = arg[i]
    end
  end

  _G.arg = new_arg

  if subcommand == "update-pr" then
    local pr = require("build.pr")
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
