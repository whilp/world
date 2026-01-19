-- box/env.tl: load env configuration

local cosmo = require("cosmo")
local path = require("cosmo.path")

local backend = require("box.backend")

local function load(): backend.Env
  local home = os.getenv("HOME")
  if not home then
    return nil
  end

  local env_path = path.join(home, ".config", "box", "env.lua")
  local content = cosmo.Slurp(env_path)
  if not content then
    return nil
  end

  local chunk, load_err = load(content, "@" .. env_path)
  if not chunk then
    io.stderr:write("warning: failed to load env: " .. (load_err or "unknown") .. "\n")
    return nil
  end

  local call_ok, env = pcall(chunk)
  if not call_ok then
    io.stderr:write("warning: failed to execute env: " .. tostring(env) .. "\n")
    return nil
  end

  return env as backend.Env
end

return {
  load = load,
}
