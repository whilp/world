-- box/run.tl: bootstrap logic that runs on the remote
--
-- Called with --local-run after box uploads itself to the remote.
-- Loads /zip/env.lua (zipped into self) for credentials.

local cosmo = require("cosmo")
local unix = require("cosmo.unix")
local path = require("cosmo.path")
local spawn = require("cosmic.spawn")

local backend = require("box.backend")

local ENV_PATH <const> = "/zip/env.lua"

local function write_file(filepath: string, content: string, mode: number): boolean, string
  local dir = path.dirname(filepath)
  if not unix.makedirs(dir) then
    return false, "failed to create directory: " .. dir
  end

  if not cosmo.Barf(filepath, content, mode or tonumber("0600", 8)) then
    return false, "failed to write: " .. filepath
  end
  return true
end

local function configure_gh(env: backend.Env): boolean, string
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

local function configure_claude(env: backend.Env): boolean, string
  if not env.claude then
    return true -- no claude config
  end

  local home = os.getenv("HOME")
  if not home then
    return false, "HOME not set"
  end

  io.stderr:write("configuring claude...\n")

  local claude_dir = path.join(home, ".claude")
  unix.makedirs(claude_dir)

  -- Write credentials if provided (Lua table -> JSON)
  local credentials = env.claude["credentials"]
  if credentials then
    local creds_file = path.join(claude_dir, ".credentials.json")
    local json = cosmo.EncodeJson(credentials)
    local ok, err = write_file(creds_file, json, tonumber("0600", 8))
    if not ok then
      return false, "failed to write credentials: " .. (err or "unknown")
    end
  end

  -- Write settings to skip onboarding
  local settings_file = path.join(claude_dir, "settings.json")
  if not unix.stat(settings_file) then
    local settings = {
      numStartups = 1,
      installMethod = "unknown",
      autoUpdates = false,
      theme = "dark-daltonized",
      hasCompletedOnboarding = true,
      bypassPermissionsModeAccepted = true,
    }
    local ok, err = write_file(settings_file, cosmo.EncodeJson(settings), tonumber("0644", 8))
    if not ok then
      return false, "failed to write settings: " .. (err or "unknown")
    end
  end

  return true
end

local function run_bootstrap(): boolean, string
  local bootstrap = require("box.bootstrap")
  local code, err = bootstrap.main()
  if code ~= 0 then
    return false, err or "bootstrap failed"
  end
  return true
end

local function write_env(): boolean, string
  local home = os.getenv("HOME")
  if not home then
    return false, "HOME not set"
  end

  local content = cosmo.Slurp(ENV_PATH)
  if not content then
    return true -- no env.lua in zip, skip
  end

  local env_path = path.join(home, ".config", "box", "env.lua")
  local ok, err = write_file(env_path, content, tonumber("0600", 8))
  if not ok then
    return false, "failed to write env.lua: " .. (err or "unknown")
  end

  return true
end

local function bootstrap(env: backend.Env): integer, string
  io.stderr:write("==> running bootstrap\n")

  -- Step 0: Write env.lua to ~/.config/box/env.lua
  local ok, err = write_env()
  if not ok then
    io.stderr:write("warning: " .. (err or "failed to write env") .. "\n")
  end

  -- Step 1: Download and unpack home
  ok, err = run_bootstrap()
  if not ok then
    return 1, err or "bootstrap failed"
  end

  -- Step 2: Configure gh auth for each github host
  ok, err = configure_gh(env)
  if not ok then
    return 1, err or "gh config failed"
  end

  -- Step 3: Configure claude credentials
  ok, err = configure_claude(env)
  if not ok then
    return 1, err or "claude config failed"
  end

  io.stderr:write("==> bootstrap complete\n")
  return 0
end

return {
  bootstrap = bootstrap,
  configure_gh = configure_gh,
  configure_claude = configure_claude,
}
