-- box/generic.tl: generic executable backend for box
--
-- Supports external executables that implement the backend interface.
-- For each method, tries exe-method first, then exe method.
-- For example: my-backend-new or my-backend new

local cosmo = require("cosmo")
local unix = require("cosmo.unix")
local path = require("cosmo.path")
local spawn = require("cosmic.spawn")

local backend_mod = require("box.backend")

local record Generic
  exe: string
end

local function exec_backend(exe: string, subcommand: string, args: {string}, passthrough: boolean): backend_mod.Result
  local cmd = {exe, subcommand}

  -- Append additional arguments
  for i = 1, #args do
    cmd[#cmd + 1] = args[i]
  end

  local opts = passthrough and {
    stdout = 1,
    stderr = 2,
  } or {}

  local handle = spawn(cmd, opts)
  if not handle then
    return {ok = false, err = "failed to spawn: " .. cmd[1]}
  end

  local exit_code = handle:wait()

  if exit_code == 0 then
    return {ok = true}
  end

  -- Capture stderr for error message if not passthrough
  local error_msg = "command failed with exit code " .. tostring(exit_code)
  if not passthrough and handle.stderr then
    local ok, stderr_content = handle.stderr:read()
    if ok and stderr_content and #(stderr_content as string) > 0 then
      error_msg = (stderr_content as string):gsub("%s+$", "")
    end
  end

  return {ok = false, err = error_msg}
end

local function new(self: Generic, name: string, ...: string): backend_mod.Result
  local args = {name}
  local extra = {...}
  for i = 1, #extra do
    args[#args + 1] = extra[i]
  end
  return exec_backend(self.exe, "new", args, false)
end

local function ssh(self: Generic, name: string, ...: string): backend_mod.Result
  local cmd = {self.exe, "ssh", name}
  local extra = {...}
  for i = 1, #extra do
    cmd[#cmd + 1] = extra[i]
  end

  -- ssh is interactive, use execvp for full passthrough
  unix.execvp(cmd[1], cmd)
  return {ok = false, err = "execvp failed"}
end

local function exec_cmd(self: Generic, name: string, cmd: string): backend_mod.Result
  return exec_backend(self.exe, "exec", {name, cmd}, true)
end

local function upload(self: Generic, name: string, src: string, dst: string): backend_mod.Result
  return exec_backend(self.exe, "upload", {name, src, dst}, false)
end

local function download(self: Generic, name: string, src: string, dst: string): backend_mod.Result
  local cmd = {self.exe, "download", name, src, dst}

  local handle = spawn(cmd, {stderr = -1})
  if not handle then
    return {ok = false, err = "failed to spawn: " .. cmd[1]}
  end

  local exit_code = handle:wait()

  if exit_code == 0 then
    return {ok = true}
  end

  -- Capture stderr for error message
  local error_msg = "download failed with exit code " .. tostring(exit_code)
  if handle.stderr then
    local ok, stderr_content = handle.stderr:read()
    if ok and stderr_content and #(stderr_content as string) > 0 then
      error_msg = (stderr_content as string):gsub("%s+$", "")
    end
  end

  return {ok = false, err = error_msg}
end

local function destroy(self: Generic, name: string): backend_mod.Result
  return exec_backend(self.exe, "destroy", {name}, false)
end

local function list(self: Generic): backend_mod.Result
  return exec_backend(self.exe, "list", {}, true)
end

local function load(exe_path: string): backend_mod.Backend, string
  local resolved_path = exe_path

  -- If path contains '/', treat as file path (absolute or relative)
  if exe_path:match("/") then
    -- Resolve relative paths to absolute
    if not exe_path:match("^/") then
      local cwd = unix.getcwd()
      if cwd then
        resolved_path = path.join(cwd, exe_path)
      end
    end

    -- Validate file exists and is executable
    local stat = unix.stat(resolved_path)
    if not stat then
      return nil, "executable not found: " .. exe_path
    end

    local mode = stat:mode()
    local is_executable = (mode & tonumber("111", 8)) ~= 0
    if not is_executable then
      return nil, "file is not executable: " .. resolved_path
    end
  end
  -- Otherwise, treat as command name - spawn/execvp will handle PATH lookup

  local g: Generic = {exe = resolved_path}

  return {
    name = "generic:" .. path.basename(resolved_path),
    new = function(...: string): backend_mod.Result return new(g, ...) end,
    ssh = function(...: string): backend_mod.Result return ssh(g, ...) end,
    exec = function(...: string): backend_mod.Result return exec_cmd(g, ...) end,
    upload = function(...: string): backend_mod.Result return upload(g, ...) end,
    download = function(...: string): backend_mod.Result return download(g, ...) end,
    destroy = function(...: string): backend_mod.Result return destroy(g, ...) end,
    list = function(): backend_mod.Result return list(g) end,
  } as backend_mod.Backend, nil
end

return {
  load = load,
}
