-- box/run.tl: remote setup logic for box
--
-- Called with --setup-env after box uploads itself to the remote.
-- Extracts /zip/env.d and sets up environment for backend bootstrap.

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

local function setup_env(name: string, kind: string): integer, string
  -- Setup environment on remote for backend bootstrap
  -- This is called by --setup-env mode after box is uploaded
  io.stderr:write("==> setting up environment\n")

  -- Load env vars from /zip/env.d
  local env_mod = require("box.env")
  local env_vars = env_mod.load_from_dir(ENV_DIR)

  -- Step 1: Write env.d to ~/.config/box/env.d
  local ok, err = write_env()
  if not ok then
    io.stderr:write("warning: " .. (err or "failed to write env") .. "\n")
  end

  -- Step 2: Write box name and kind
  ok, err = write_box_info(name, kind)
  if not ok then
    io.stderr:write("warning: " .. (err or "failed to write box info") .. "\n")
  end

  -- Step 3: Set environment variables from env files
  if env_vars then
    for k, v in pairs(env_vars) do
      unix.setenv(k, v)
    end
  end

  io.stderr:write("==> environment setup complete\n")
  return 0
end

return {
  setup_env = setup_env,
  -- Keep these for potential reuse
  write_env = write_env,
  write_box_info = write_box_info,
}
