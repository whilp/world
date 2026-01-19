-- box/git.tl: configure git identity

local spawn = require("cosmic.spawn")

local function configure(): boolean, string
  local name = os.getenv("GIT_NAME")
  local email = os.getenv("GIT_EMAIL")

  if name then
    io.stderr:write("configuring git user.name...\n")
    local handle = spawn({"git", "config", "--global", "user.name", name})
    local exit_code = handle:wait()
    if exit_code ~= 0 then
      io.stderr:write("warning: git config user.name failed (exit " .. tostring(exit_code) .. ")\n")
    end
  end

  if email then
    io.stderr:write("configuring git user.email...\n")
    local handle = spawn({"git", "config", "--global", "user.email", email})
    local exit_code = handle:wait()
    if exit_code ~= 0 then
      io.stderr:write("warning: git config user.email failed (exit " .. tostring(exit_code) .. ")\n")
    end
  end

  return true
end

return {
  configure = configure,
}
