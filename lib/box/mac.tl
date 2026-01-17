-- box/mac.tl: local mac backend for testing
--
-- Runs commands locally with no isolation. Useful for testing the bootstrap
-- flow without needing a remote sprite.

local unix = require("cosmo.unix")
local path = require("cosmo.path")
local spawn = require("cosmic.spawn")

local backend = require("box.backend")

local function ok(): backend.Result
  return {ok = true}
end

local function err(msg: string): backend.Result
  return {ok = false, err = msg}
end

-- Boxes are tracked in ~/.config/box/boxes/
local function get_box_dir(): string
  local home = os.getenv("HOME")
  if not home then return nil end
  return path.join(home, ".config", "box", "boxes")
end

local function get_box_path(name: string): string
  local box_dir = get_box_dir()
  if not box_dir then return nil end
  return path.join(box_dir, name)
end

local function new(name: string, ...: string): backend.Result
  local box_path = get_box_path(name)
  if not box_path then
    return err("cannot determine box path")
  end

  -- Check if already exists
  local st = unix.stat(box_path)
  if st then
    return err("box already exists: " .. name)
  end

  -- Create marker directory
  if not unix.makedirs(box_path) then
    return err("failed to create box directory")
  end

  io.stderr:write("created local box: " .. name .. "\n")
  return ok()
end

local function ssh(name: string, ...: string): backend.Result
  local box_path = get_box_path(name)
  if not box_path then
    return err("cannot determine box path")
  end

  local st = unix.stat(box_path)
  if not st then
    return err("box not found: " .. name)
  end

  -- Just spawn a shell
  local shell = os.getenv("SHELL") or "/bin/bash"
  local args = {shell}
  for i = 1, select("#", ...) do
    local a = select(i, ...)
    table.insert(args, a)
  end

  unix.execvp(shell, args)
  return err("failed to exec shell")
end

local function exec(name: string, cmd: string): backend.Result
  local box_path = get_box_path(name)
  if not box_path then
    return err("cannot determine box path")
  end

  local st = unix.stat(box_path)
  if not st then
    return err("box not found: " .. name)
  end

  local handle = spawn({"bash", "-c", cmd})
  local exit_code = handle:wait()
  if exit_code ~= 0 then
    return err("exec failed: exit " .. tostring(exit_code))
  end
  return ok()
end

local function upload(name: string, src: string, dst: string): backend.Result
  local box_path = get_box_path(name)
  if not box_path then
    return err("cannot determine box path")
  end

  local st = unix.stat(box_path)
  if not st then
    return err("box not found: " .. name)
  end

  -- For local mac, just copy the file
  local src_f = io.open(src, "rb")
  if not src_f then
    return err("failed to open source: " .. src)
  end
  local data = src_f:read("*a")
  src_f:close()

  -- Ensure destination directory exists
  local dst_dir = path.dirname(dst)
  unix.makedirs(dst_dir)

  local dst_f = io.open(dst, "wb")
  if not dst_f then
    return err("failed to open destination: " .. dst)
  end
  dst_f:write(data)
  dst_f:close()

  unix.chmod(dst, tonumber("755", 8))
  return ok()
end

local function destroy(name: string): backend.Result
  local box_path = get_box_path(name)
  if not box_path then
    return err("cannot determine box path")
  end

  local st = unix.stat(box_path)
  if not st then
    return ok() -- already gone
  end

  if not unix.rmrf(box_path) then
    return err("failed to remove box directory")
  end

  io.stderr:write("destroyed local box: " .. name .. "\n")
  return ok()
end

local function list(): backend.Result
  local box_dir = get_box_dir()
  if not box_dir then
    return err("cannot determine box directory")
  end

  local st = unix.stat(box_dir)
  if not st then
    return ok() -- no boxes
  end

  local dir_iter = unix.opendir(box_dir)
  if not dir_iter then
    return ok()
  end

  for entry in dir_iter do
    if entry ~= "." and entry ~= ".." then
      io.stdout:write(entry .. "\n")
    end
  end

  return ok()
end

return {
  name = "mac",
  new = new,
  ssh = ssh,
  exec = exec,
  upload = upload,
  destroy = destroy,
  list = list,
} as backend.Backend
