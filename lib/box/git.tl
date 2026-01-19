-- box/git.tl: configure git identity

local spawn = require("cosmic.spawn")

local backend = require("box.backend")

local function configure(env: backend.Env): boolean, string
  if not env.git then
    return true -- no git config
  end

  if env.git.name then
    io.stderr:write("configuring git user.name...\n")
    local handle = spawn({"git", "config", "--global", "user.name", env.git.name})
    local exit_code = handle:wait()
    if exit_code ~= 0 then
      io.stderr:write("warning: git config user.name failed (exit " .. tostring(exit_code) .. ")\n")
    end
  end

  if env.git.email then
    io.stderr:write("configuring git user.email...\n")
    local handle = spawn({"git", "config", "--global", "user.email", env.git.email})
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
