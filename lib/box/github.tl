-- box/github.tl: configure gh auth

local unix = require("cosmo.unix")
local path = require("cosmo.path")
local spawn = require("cosmic.spawn")

local backend = require("box.backend")

local function configure(env: backend.Env): boolean, string
  if not env.github then
    return true -- no github config
  end

  local home = os.getenv("HOME")
  if not home then
    return false, "HOME not set"
  end

  local gh_bin = path.join(home, ".local", "share", "gh", "bin", "gh")
  local st = unix.stat(gh_bin)
  if not st then
    io.stderr:write("warning: gh not found at " .. gh_bin .. "\n")
    return true -- not an error, just skip
  end

  for host, token in pairs(env.github) do
    io.stderr:write("configuring gh auth for " .. host .. "...\n")

    local args: {string} = {gh_bin, "auth", "login", "-h", host, "--with-token"}
    local handle = spawn(args)

    if handle.stdin then
      handle.stdin:write(token .. "\n")
      handle.stdin:close()
    end

    local exit_code = handle:wait()
    if exit_code ~= 0 then
      io.stderr:write("warning: gh auth login failed for " .. host .. " (exit " .. tostring(exit_code) .. ")\n")
    end
  end

  return true
end

return {
  configure = configure,
}
