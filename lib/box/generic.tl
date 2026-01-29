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

local record ExecOpts
  name: string
  src: string
  dst: string
  cmd: string
end

local function build_env(opts: ExecOpts): {string}
  local env = unix.environ()
  if opts then
    if opts.name then env[#env + 1] = "BOX_NAME=" .. opts.name end
    if opts.src then env[#env + 1] = "BOX_SRC=" .. opts.src end
    if opts.dst then env[#env + 1] = "BOX_DST=" .. opts.dst end
    if opts.cmd then env[#env + 1] = "BOX_CMD=" .. opts.cmd end
  end
  return env
end

local function exec_backend(exe: string, subcommand: string, args: {string}, passthrough: boolean, box_opts: ExecOpts): backend.Result
  local cmd = {exe, subcommand}

  -- Append additional arguments
  for i = 1, #args do
    cmd[#cmd + 1] = args[i]
  end

  local env = build_env(box_opts)
  local opts = passthrough and {
    stdin = 0,
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
  local args = {...}
  -- passthrough=true so user sees progress and error messages
  return exec_backend(self.exe, "new", args, true, {name = name})
end

local function ssh(self: Generic, name: string, ...: string): backend.Result
  local cmd = {self.exe, "ssh"}
  local extra = {...}
  for i = 1, #extra do
    cmd[#cmd + 1] = extra[i]
  end

  -- Set BOX_NAME in environment
  unix.setenv("BOX_NAME", name)

  -- ssh is interactive, use execvp for full passthrough
  unix.execvp(cmd[1], cmd)
  return {ok = false, err = "execvp failed"}
end

local function exec_cmd(self: Generic, name: string, cmd_str: string): backend.Result
  return exec_backend(self.exe, "exec", {}, true, {name = name, cmd = cmd_str})
end

-- Helper to execute and capture stdout
local function exec_capture(exe: string, name: string, cmd_str: string): string, string
  local cmd = {exe, "exec"}
  local env = build_env({name = name, cmd = cmd_str})
  local handle = spawn(cmd, {env = env})
  if not handle then
    return nil, "failed to spawn"
  end
  local stdout_content: string
  if handle.stdout then
    local ok, content = handle.stdout:read()
    if ok and content then
      stdout_content = (content as string):gsub("%s+$", "")
    end
  end
  if handle.stderr then handle.stderr:read() end
  local exit_code = handle:wait()
  if exit_code ~= 0 then
    return nil, "command failed"
  end
  return stdout_content
end

-- Resolve relative path to absolute using remote $HOME
local function resolve_remote_path(exe: string, name: string, p: string): string, string
  if p:sub(1, 1) == "/" then
    return p  -- already absolute
  end
  local home, err = exec_capture(exe, name, "printf %s \"$HOME\"")
  if not home then
    return nil, err
  end
  if p == "." or p == "./" then
    return home
  elseif p:sub(1, 2) == "./" then
    return path.join(home, p:sub(3))
  else
    return path.join(home, p)
  end
end

local function upload(self: Generic, name: string, src: string, dst: string): backend.Result
  -- Resolve relative dst to absolute
  local resolved_dst, resolve_err = resolve_remote_path(self.exe, name, dst)
  if not resolved_dst then
    return {ok = false, err = "failed to resolve destination path: " .. (resolve_err or "unknown")}
  end
  return exec_backend(self.exe, "upload", {}, false, {name = name, src = src, dst = resolved_dst})
end

local function download(self: Generic, name: string, src: string, dst: string): backend.Result
  return exec_backend(self.exe, "download", {}, false, {name = name, src = src, dst = dst})
end

local function destroy(self: Generic, name: string): backend.Result
  return exec_backend(self.exe, "destroy", {}, false, {name = name})
end

local function list(self: Generic): backend.Result
  return exec_backend(self.exe, "list", {}, true, nil)
end

local function exists(self: Generic, name: string): boolean
  local result = exec_backend(self.exe, "exists", {}, false, {name = name})
  return result.ok
end

local function bootstrap(self: Generic, name: string): backend.Result
  return exec_backend(self.exe, "bootstrap", {}, true, {name = name})
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
