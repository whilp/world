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

local function run_bootstrap_script(): boolean, string
  local script_path = "/zip/bootstrap"
  local stat = unix.stat(script_path)
  if not stat then
    return true -- no script to run
  end

  io.stderr:write("==> running backend bootstrap\n")

  -- Set up box in PATH so scripts can use #!/usr/bin/env box
  -- Create temp directory for box symlink
  local tmpdir = unix.mkdtemp("/tmp/box-bootstrap-XXXXXX")
  if not tmpdir then
    return false, "failed to create temp directory for bootstrap"
  end

  -- Symlink /tmp/box to tmpdir/box
  local box_link = path.join(tmpdir, "box")
  if not unix.symlink("/tmp/box", box_link) then
    unix.rmrf(tmpdir)
    return false, "failed to create box symlink"
  end

  -- Execute script with modified PATH and interpreter mode flag
  local spawn = require("cosmic.spawn")
  local old_path = os.getenv("PATH") or ""
  local new_path = tmpdir .. ":" .. old_path

  local handle = spawn({"env", "PATH=" .. new_path, "BOX_INTERPRETER_MODE=1", script_path}, {
    stdout = 1,
    stderr = 2,
  })

  if not handle then
    unix.rmrf(tmpdir)
    return false, "failed to spawn bootstrap script"
  end

  local exit_code = handle:wait()
  unix.rmrf(tmpdir)

  if exit_code ~= 0 then
    return false, "bootstrap script failed with exit code " .. tostring(exit_code)
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
  -- All backends now use the bootstrap script approach
  ok, err = run_bootstrap_script()
  if not ok then
    return 1, err or "bootstrap script failed"
  end

  io.stderr:write("==> bootstrap complete\n")
  return 0
end

return {
  bootstrap = bootstrap,
}
