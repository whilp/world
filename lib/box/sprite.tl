-- box/sprite.tl: sprites.dev backend for box

local unix = require("cosmo.unix")
local spawn = require("cosmic.spawn")

local backend = require("box.backend")

local function ok(): backend.Result
  return {ok = true}
end

local function err(msg: string): backend.Result
  return {ok = false, err = msg}
end

local function new(name: string, ...: string): backend.Result
  -- destroy if exists (ignore errors)
  local destroy_handle = spawn({"sprite", "destroy", "-s", name, "-force"})
  if destroy_handle then
    destroy_handle:wait()
  end

  -- create new sprite
  local handle = spawn({"sprite", "create", name})
  if not handle then
    return err("sprite binary not found")
  end
  local exit_code = handle:wait()

  -- sprites.dev sometimes returns errors but still creates the sprite
  -- check if it exists
  local list_handle = spawn({"sprite", "list"})
  if list_handle then
    local list_ok, list_out = list_handle:read()
    if list_ok and list_out and (list_out as string):find(name, 1, true) then
      return ok()
    end
  end

  if exit_code ~= 0 then
    return err("failed to create sprite: exit " .. tostring(exit_code))
  end

  return ok()
end

local function ssh(name: string, ...: string): backend.Result
  local args = {"sprite", "console", "-s", name}
  for i = 1, select("#", ...) do
    local a = select(i, ...)
    table.insert(args, a)
  end

  unix.execvp("sprite", args)
  return err("failed to exec sprite console")
end

local function exec(name: string, cmd: string): backend.Result
  local handle = spawn({"sprite", "exec", "-s", name, "--", "bash", "-c", cmd})
  if not handle then
    return err("sprite binary not found")
  end
  local exit_code = handle:wait()
  if exit_code ~= 0 then
    return err("exec failed: exit " .. tostring(exit_code))
  end
  return ok()
end

local function upload(name: string, src: string, dst: string): backend.Result
  local file_arg = src .. ":" .. dst
  local handle = spawn({"sprite", "exec", "-s", name, "-file", file_arg, "--", "true"})
  if not handle then
    return err("sprite binary not found")
  end
  local exit_code = handle:wait()
  if exit_code ~= 0 then
    return err("upload failed: exit " .. tostring(exit_code))
  end
  return ok()
end

local function destroy(name: string): backend.Result
  local handle = spawn({"sprite", "destroy", "-s", name, "-force"})
  if not handle then
    return err("sprite binary not found")
  end
  local exit_code = handle:wait()
  if exit_code ~= 0 then
    return err("destroy failed: exit " .. tostring(exit_code))
  end
  return ok()
end

local function list(): backend.Result
  unix.execvp("sprite", {"sprite", "list"})
  return err("failed to exec sprite list")
end

return {
  name = "sprite",
  new = new,
  ssh = ssh,
  exec = exec,
  upload = upload,
  destroy = destroy,
  list = list,
} as backend.Backend
