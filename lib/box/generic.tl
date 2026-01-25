-- box/generic.tl: generic executable backend for box
--
-- Supports external executables that implement the backend interface.

local unix = require("cosmo.unix")
local path = require("cosmo.path")
local spawn = require("cosmic.spawn")

local backend = require("box.backend")

local record Generic
  exe: string
end

local function exec_backend(exe: string, subcommand: string, args: {string}, passthrough: boolean): backend.Result
  local cmd = {exe, subcommand}

  -- Append additional arguments
  for i = 1, #args do
    cmd[#cmd + 1] = args[i]
  end

  local env = unix.environ()
  local opts = passthrough and {
    stdout = 1,
    stderr = 2,
    env = env,
  } or {
    env = env,
  }

  local handle = spawn(cmd, opts)
  if not handle then
    return {ok = false, err = "failed to spawn: " .. cmd[1]}
  end

  -- Drain pipes before wait() to avoid deadlock
  -- Note: closing fds (-1) breaks some lua/teal programs, so we use pipes
  local stderr_content: string
  if not passthrough then
    if handle.stdout then handle.stdout:read() end
    if handle.stderr then
      local read_result = {handle.stderr:read()}
      if read_result[1] then
        stderr_content = read_result[2] as string
      end
    end
  end

  local exit_code = handle:wait()

  if exit_code == 0 then
    return {ok = true}
  end

  local error_msg = "command failed with exit code " .. tostring(exit_code)
  if stderr_content and #stderr_content > 0 then
    error_msg = stderr_content:gsub("%s+$", "")
  end

  return {ok = false, err = error_msg}
end

local function new(self: Generic, name: string, ...: string): backend.Result
  local args = {name}
  local extra = {...}
  for i = 1, #extra do
    args[#args + 1] = extra[i]
  end
  -- passthrough=true so user sees progress and error messages
  return exec_backend(self.exe, "new", args, true)
end

local function ssh(self: Generic, name: string, ...: string): backend.Result
  local cmd = {self.exe, "ssh", name}
  local extra = {...}
  for i = 1, #extra do
    cmd[#cmd + 1] = extra[i]
  end

  -- ssh is interactive, use execvp for full passthrough
  unix.execvp(cmd[1], cmd)
  return {ok = false, err = "execvp failed"}
end

local function exec_cmd(self: Generic, name: string, cmd: string): backend.Result
  return exec_backend(self.exe, "exec", {name, cmd}, true)
end

local function upload(self: Generic, name: string, src: string, dst: string): backend.Result
  return exec_backend(self.exe, "upload", {name, src, dst}, false)
end

local function download(self: Generic, name: string, src: string, dst: string): backend.Result
  return exec_backend(self.exe, "download", {name, src, dst}, false)
end

local function destroy(self: Generic, name: string): backend.Result
  return exec_backend(self.exe, "destroy", {name}, false)
end

local function list(self: Generic): backend.Result
  return exec_backend(self.exe, "list", {}, true)
end

local function exists(self: Generic, name: string): boolean
  local result = exec_backend(self.exe, "exists", {name}, false)
  return result.ok
end

local function bootstrap(self: Generic, name: string): backend.Result
  return exec_backend(self.exe, "bootstrap", {name}, true)
end

local function load(exe_path: string): backend.Backend, string
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
    new = function(name: string, ...: string): backend.Result return new(g, name, ...) end,
    ssh = function(name: string, ...: string): backend.Result return ssh(g, name, ...) end,
    exec = function(name: string, cmd: string): backend.Result return exec_cmd(g, name, cmd) end,
    upload = function(name: string, src: string, dst: string): backend.Result return upload(g, name, src, dst) end,
    download = function(name: string, src: string, dst: string): backend.Result return download(g, name, src, dst) end,
    destroy = function(name: string): backend.Result return destroy(g, name) end,
    list = function(): backend.Result return list(g) end,
    exists = function(name: string): boolean return exists(g, name) end,
    bootstrap = function(name: string): backend.Result return bootstrap(g, name) end,
  } as backend.Backend, nil
end

return {
  load = load,
}
