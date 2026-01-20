-- box/run.tl: bootstrap logic that runs on the remote
--
-- Called with --local-run after box uploads itself to the remote.
-- Loads /zip/env.d (zipped into self) for environment variables.

local cosmo = require("cosmo")
local unix = require("cosmo.unix")
local path = require("cosmo.path")

local ENV_DIR <const> = "/zip/env.d"

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

local function run_bootstrap(): boolean, string
  local bootstrap = require("box.bootstrap")
  local code, err = bootstrap.main()
  if code ~= 0 then
    return false, err or "bootstrap failed"
  end
  return true
end

local function get_box_dir(): string
  local home = os.getenv("HOME")
  if not home then
    return nil
  end
  return path.join(home, ".config", "box")
end

local function write_env(): boolean, string
  local box_dir = get_box_dir()
  if not box_dir then
    return false, "HOME not set"
  end

  -- Check if env.d exists in zip
  local stat = unix.stat(ENV_DIR)
  if not stat then
    return true -- no env.d in zip, skip
  end

  -- Ensure box_dir exists
  if not unix.makedirs(box_dir) then
    return false, "failed to create box config directory"
  end

  -- Create temp dir with mkdtemp for atomic write
  local tmp_dir = unix.mkdtemp(path.join(box_dir, "env-XXXXXX"))
  if not tmp_dir then
    return false, "failed to create temp env directory"
  end

  -- Copy all files from /zip/env.d to temp dir
  local handle = unix.opendir(ENV_DIR)
  if not handle then
    unix.rmrf(tmp_dir)
    return false, "failed to read env.d"
  end

  for entry in handle do
    if entry ~= "." and entry ~= ".." then
      local src = path.join(ENV_DIR, entry)
      local dst = path.join(tmp_dir, entry)
      local src_stat = unix.stat(src)
      if src_stat and unix.S_ISREG(src_stat:mode()) then
        local content = cosmo.Slurp(src)
        if content then
          local ok, write_err = write_file(dst, content, tonumber("600", 8))
          if not ok then
            unix.rmrf(tmp_dir)
            return false, "failed to write " .. entry .. ": " .. (write_err or "unknown")
          end
        end
      end
    end
  end

  -- Atomically rename temp dir to env.d
  local env_dir = path.join(box_dir, "env.d")
  unix.rmrf(env_dir)
  if not unix.rename(tmp_dir, env_dir) then
    unix.rmrf(tmp_dir)
    return false, "failed to rename env directory"
  end

  return true
end

local function write_box_info(name: string, kind: string): boolean, string
  local box_dir = get_box_dir()
  if not box_dir then
    return false, "HOME not set"
  end

  if name and name ~= "" then
    local ok, err = write_file(path.join(box_dir, "name"), name .. "\n", tonumber("0644", 8))
    if not ok then
      return false, "failed to write name: " .. (err or "unknown")
    end
  end

  if kind and kind ~= "" then
    local ok, err = write_file(path.join(box_dir, "kind"), kind .. "\n", tonumber("0644", 8))
    if not ok then
      return false, "failed to write kind: " .. (err or "unknown")
    end
  end

  return true
end

local backend = require("box.backend")

local function load_backend(kind: string): backend.Backend
  if kind == "sprite" then
    return require("box.sprite") as backend.Backend
  elseif kind == "mac" then
    return require("box.mac") as backend.Backend
  end
  return nil
end

local function run_bootstrap_script(): boolean, string
  local script_path = "/zip/bootstrap.sh"
  local stat = unix.stat(script_path)
  if not stat then
    return true -- no script to run
  end

  -- Read script to detect shebang
  local script_content = cosmo.Slurp(script_path)
  if not script_content then
    return false, "failed to read bootstrap script"
  end

  -- Check shebang to determine how to execute
  local shebang = script_content:match("^#!([^\n]+)")
  if not shebang then
    return false, "bootstrap script missing shebang"
  end

  io.stderr:write("==> running generic backend bootstrap\n")

  -- If shebang contains "lua", execute as Lua code
  if shebang:match("lua") then
    -- Execute as Lua with access to box modules
    local chunk, load_err = load(script_content, "@/zip/bootstrap.sh")
    if not chunk then
      return false, "failed to load bootstrap script: " .. (load_err or "unknown")
    end
    local ok, run_err = pcall(chunk)
    if not ok then
      return false, "bootstrap script failed: " .. (run_err or "unknown")
    end
  else
    -- Execute as shell script
    local spawn = require("cosmic.spawn")
    local handle = spawn({"/bin/sh", script_path}, {
      stdout = 1,
      stderr = 2,
    })
    if not handle then
      return false, "failed to spawn bootstrap script"
    end
    local exit_code = handle:wait()
    if exit_code ~= 0 then
      return false, "bootstrap script failed with exit code " .. tostring(exit_code)
    end
  end

  return true
end

local function bootstrap(env_vars: {string:string}, name: string, kind: string): integer, string
  io.stderr:write("==> running bootstrap\n")

  -- Step 0: Write env.d to ~/.config/box/env.d
  local ok, err = write_env()
  if not ok then
    io.stderr:write("warning: " .. (err or "failed to write env") .. "\n")
  end

  -- Step 0.5: Write box name and kind
  ok, err = write_box_info(name, kind)
  if not ok then
    io.stderr:write("warning: " .. (err or "failed to write box info") .. "\n")
  end

  -- Step 0.75: Set environment variables from env files
  for k, v in pairs(env_vars) do
    unix.setenv(k, v)
  end

  -- Step 1: Download and unpack home
  ok, err = run_bootstrap()
  if not ok then
    return 1, err or "bootstrap failed"
  end

  -- Step 2: Run backend-specific bootstrap (installs claude, configures credentials)
  if kind and kind ~= "" then
    local be = load_backend(kind)
    if be and be.bootstrap then
      io.stderr:write("==> running backend bootstrap\n")
      local result = be.bootstrap(name)
      if not result.ok then
        return 1, result.err or "backend bootstrap failed"
      end
    elseif not be then
      -- Backend couldn't be loaded (generic backend), try bundled script
      ok, err = run_bootstrap_script()
      if not ok then
        return 1, err or "generic bootstrap script failed"
      end
    end
  end

  io.stderr:write("==> bootstrap complete\n")
  return 0
end

return {
  bootstrap = bootstrap,
}
