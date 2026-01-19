local unix = require("cosmo.unix")
local path = require("cosmo.path")
local version = require("version")
local whereami = require("whereami")
local daemonize = require("daemonize")

local HOME <const> = os.getenv("HOME")
local DEFAULT_SOCK <const> = path.join(HOME, ".config", "nvim", "nvim.sock")

local record Paths
  sock: string
  pid: string
  log: string
end

local type CommandFn = function(paths: Paths, nvim_bin?: string): number

local function resolve_nvim_bin(): string, string
  local bin_path = version.resolve_bin("nvim")
  if bin_path then
    return bin_path
  end

  local fallback = path.join(HOME, ".local", "share", "nvim", "bin", "nvim")
  if unix.stat(fallback) then
    return fallback
  end

  return nil, "nvim binary not found. Run 'make nvim' to install."
end

local function current_environ(): {string}
  local result: {string} = {}
  for key, value in pairs(unix.environ()) do
    table.insert(result, key .. "=" .. value)
  end
  return result
end

local function derive_paths(sock: string): Paths
  local sock_dir = path.dirname(sock)
  local sock_name = path.basename(sock):gsub("%.sock$", "")

  return {
    sock = sock,
    pid = path.join(sock_dir, sock_name .. ".pid"),
    log = path.join(sock_dir, sock_name .. ".log")
  }
end

local function extract_server_socket(args: {string}): string
  for i = 1, #args do
    if args[i] == "--server" and i < #args then
      return args[i + 1]
    end
  end
  return nil
end

local function expand_tilde(file_path: string): string
  if file_path:sub(1, 2) == "~/" then
    return path.join(HOME, file_path:sub(3))
  elseif file_path == "~" then
    return HOME
  end
  return file_path
end

local function get_socket_path(args: {string}): string
  local server_socket = extract_server_socket(args)
  if server_socket then
    return expand_tilde(server_socket)
  end

  local env_socket = os.getenv("NVIM_SOCKET")
  if env_socket then
    return expand_tilde(env_socket)
  end

  return DEFAULT_SOCK
end

local function mkdir_p(dir_path: string): boolean, string
  local ok, err = unix.makedirs(dir_path, tonumber("755", 8))
  if not ok then
    return nil, "failed to create directory " .. dir_path .. ": " .. tostring(err)
  end
  return true
end

local function file_exists(file_path: string): boolean
  return unix.stat(file_path) ~= nil
end

local function read_file(file_path: string): string
  local f = io.open(file_path, "r")
  if not f then
    return nil
  end
  local content = f:read("*all")
  f:close()
  return content
end

local function is_process_running(pid: number): boolean
  local ok = unix.kill(pid, 0)
  return ok ~= nil
end

local function get_running_pid(pidfile: string): number
  local pid_str = read_file(pidfile)
  if not pid_str then
    return nil
  end
  local pid = tonumber(pid_str)
  if pid and is_process_running(pid) then
    return pid
  end
  return nil
end

local function wait_for_socket(socket_path: string, timeout?: number): boolean
  timeout = timeout or 5
  local sleep_interval_ms: number = 100
  local elapsed: number = 0

  while elapsed < timeout do
    if file_exists(socket_path) then
      local fd = unix.socket(unix.AF_UNIX, unix.SOCK_STREAM)
      if fd and fd >= 0 then
        local ok = unix.connect(fd, socket_path)
        unix.close(fd)
        if ok then
          return true
        end
      end
    end

    unix.nanosleep(0, sleep_interval_ms * 1000000)
    elapsed = elapsed + (sleep_interval_ms / 1000)
  end

  return false
end

local function setup_nvim_environment(nvim_bin: string): {string}
  local env_table: {string:string} = {}
  for key, value in pairs(unix.environ()) do
    env_table[key] = value
  end
  env_table["NVIM_SERVER_MODE"] = "1"
  env_table["WHEREAMI"] = whereami.get_with_emoji()
  local env: {string} = {}
  for k, v in pairs(env_table) do
    table.insert(env, k .. "=" .. v)
  end
  return env
end

local function exec_nvim_server(sock: string, env: {string}, nvim_bin: string)
  os.remove(sock)
  unix.execve(nvim_bin, {"nvim", "--listen", sock, "--headless"}, env)
end

local function cmd_start(paths: Paths, nvim_bin: string): number
  local ok, err = mkdir_p(path.dirname(paths.pid))
  if not ok then
    io.stderr:write("error: " .. err .. "\n")
    return 1
  end
  ok, err = mkdir_p(path.dirname(paths.log))
  if not ok then
    io.stderr:write("error: " .. err .. "\n")
    return 1
  end

  local lockfd: number
  lockfd, err = daemonize.acquire_lock(paths.pid)
  if not lockfd then
    if err:match("another instance") then
      io.write("nvim server already running\n")
      return 0
    end
    io.stderr:write("failed to acquire lock: " .. err .. "\n")
    return 1
  end

  os.remove(paths.sock)

  local child_pid = unix.fork()
  if child_pid == 0 then
    ok = unix.daemon(true, true)
    if not ok then
      io.stderr:write("daemon failed\n")
      unix.exit(1)
    end

    ok, err = daemonize.redirect_output(paths.log, paths.log, true)
    if not ok then
      io.stderr:write("redirect_output failed: " .. err .. "\n")
      unix.exit(1)
    end

    local devnull = unix.open("/dev/null", unix.O_RDONLY)
    if devnull then
      unix.dup(devnull, 0)
      unix.close(devnull)
    end

    ok, err = daemonize.write_pidfile(paths.pid)
    if not ok then
      io.stderr:write("write_pidfile failed: " .. err .. "\n")
      unix.exit(1)
    end

    unix.close(lockfd)

    unix.chdir(HOME)
    local env = setup_nvim_environment(nvim_bin)
    exec_nvim_server(paths.sock, env, nvim_bin)
    unix.exit(1)
  elseif child_pid > 0 then
    unix.close(lockfd)
    unix.wait()

    if wait_for_socket(paths.sock, 5) then
      local running_pid = get_running_pid(paths.pid)
      if running_pid then
        io.write("nvim server started\n")
        return 0
      else
        io.stderr:write("nvim server process died after starting, check " .. paths.log .. "\n")
        os.remove(paths.pid)
        return 1
      end
    else
      io.stderr:write("nvim server failed to start: socket not ready after 5 seconds, check " .. paths.log .. "\n")
      local running_pid = get_running_pid(paths.pid)
      if running_pid then
        unix.kill(running_pid, unix.SIGTERM)
      end
      os.remove(paths.pid)
      return 1
    end
  else
    unix.close(lockfd)
    io.stderr:write("fork failed\n")
    return 1
  end
end

local function cmd_stop(paths: Paths): number
  local pid = get_running_pid(paths.pid)
  if pid then
    unix.kill(pid, unix.SIGTERM)
    os.remove(paths.pid)
    io.write("nvim server stopped\n")
  else
    io.write("nvim server not running\n")
  end
  os.remove(paths.sock)
  return 0
end

local function cmd_status(paths: Paths): number
  local pid = get_running_pid(paths.pid)
  if pid then
    io.write(string.format("nvim server is running (pid %d) at %s\n", pid as integer, paths.sock))
    return 0
  else
    io.write(string.format("nvim server is not running at %s\n", paths.sock))
    return 1
  end
end

local commands: {string:CommandFn} = {
  start = cmd_start,
  stop = cmd_stop,
  status = cmd_status,
}

local function daemon_mode(args: {string}, nvim_bin: string): number
  local socket_path = get_socket_path(args)
  local paths = derive_paths(socket_path)

  local command = args[1] or ""
  for _, arg in ipairs(args) do
    if arg ~= "--server" and not arg:match("^/") and not arg:match("^~") then
      command = arg
      break
    end
  end

  local cmd_fn = commands[command]

  if cmd_fn then
    if command == "start" then
      return cmd_fn(paths, nvim_bin)
    else
      return cmd_fn(paths)
    end
  else
    io.stderr:write("nvimd: unknown command: " .. command .. "\n")
    io.stderr:write("usage: nvimd [--server PATH] {start|stop|status}\n")
    return 1
  end
end

local function has_remote_flag(args: {string}): boolean
  for _, arg in ipairs(args) do
    if arg == "--server" or arg:match("^%-%-remote%-") then
      return true
    end
  end
  return false
end

local function build_nvim_args(args: {string}): {string}
  local new_args: {string} = {"nvim"}
  for _, arg in ipairs(args) do
    table.insert(new_args, arg)
  end
  return new_args
end

local function ensure_server_running(paths: Paths, nvim_bin: string)
  if not file_exists(paths.sock) then
    cmd_start(paths, nvim_bin)
  end
end

local function client_mode(args: {string}, nvim_bin: string)
  local nvim_invim = os.getenv("NVIM_INVIM")
  local nvim_server_mode = os.getenv("NVIM_SERVER_MODE")
  local env = current_environ()

  if nvim_invim or nvim_server_mode then
    unix.execve(nvim_bin, build_nvim_args(args), env)
    return
  end

  local use_remote = has_remote_flag(args)

  if use_remote then
    local socket_path = get_socket_path(args)
    local paths = derive_paths(socket_path)

    ensure_server_running(paths, nvim_bin)

    local has_server_arg = extract_server_socket(args) ~= nil

    if not has_server_arg then
      local new_args: {string} = {"nvim", "--server", paths.sock}
      for _, arg in ipairs(args) do
        table.insert(new_args, arg)
      end
      unix.execve(nvim_bin, new_args, env)
    else
      unix.execve(nvim_bin, build_nvim_args(args), env)
    end
  else
    unix.execve(nvim_bin, build_nvim_args(args), env)
  end
end

local type Args = {number: string}

local function main(args: Args): number
  local program_name = args[0]:match("([^/]+)$")
  local nvim_bin, err = resolve_nvim_bin()
  if not nvim_bin then
    io.stderr:write("error: " .. err .. "\n")
    return 1
  end

  local cmd_args: {string} = {}
  for i = 1, #args do
    cmd_args[i] = args[i]
  end

  if program_name == "nvimd" or program_name == "nvimd.lua" then
    return daemon_mode(cmd_args, nvim_bin)
  else
    return client_mode(cmd_args, nvim_bin)
  end
end

return {
  main = main,
  setup_nvim_environment = setup_nvim_environment,
  resolve_nvim_bin = resolve_nvim_bin,
}
