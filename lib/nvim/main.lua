local unix = require("cosmo.unix")
local path = require("cosmo.path")
local version = require("version")
local whereami = require("whereami")
local daemonize = require("daemonize")

local HOME = os.getenv("HOME")
local DEFAULT_SOCK = path.join(HOME, ".config", "nvim", "nvim.sock")

local function resolve_nvim_bin()
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

local function derive_vimruntime(nvim_bin)
  local bin_dir = path.dirname(nvim_bin)
  local version_dir = path.dirname(bin_dir)
  return path.join(version_dir, "share", "nvim", "runtime")
end

local function set_vimruntime(env, nvim_bin)
  local vimruntime = derive_vimruntime(nvim_bin)
  if unix.stat(vimruntime) then
    env["VIMRUNTIME"] = vimruntime
  end
end

local function env_with_vimruntime(nvim_bin)
  local env = {}
  for _, entry in ipairs(unix.environ()) do
    local key, value = entry:match("^([^=]+)=(.*)$")
    if key then
      env[key] = value
    end
  end
  set_vimruntime(env, nvim_bin)
  local result = {}
  for k, v in pairs(env) do
    table.insert(result, k .. "=" .. v)
  end
  return result
end

local function derive_paths(sock)
  local sock_dir = path.dirname(sock)
  local sock_name = path.basename(sock):gsub("%.sock$", "")

  return {
    sock = sock,
    pid = path.join(sock_dir, sock_name .. ".pid"),
    log = path.join(sock_dir, sock_name .. ".log")
  }
end

local function parse_socket_option(args)
  local socket_path = nil
  local new_args = {}
  local i = 1

  while i <= #args do
    if args[i] == "--socket" and i < #args then
      socket_path = args[i + 1]
      i = i + 2
    else
      table.insert(new_args, args[i])
      i = i + 1
    end
  end

  return socket_path, new_args
end

local function expand_tilde(file_path)
  if file_path:sub(1, 2) == "~/" then
    return path.join(HOME, file_path:sub(3))
  elseif file_path == "~" then
    return HOME
  end
  return file_path
end

local function get_socket_path(cli_socket)
  if cli_socket then
    return expand_tilde(cli_socket)
  end

  local env_socket = os.getenv("NVIM_SOCKET")
  if env_socket then
    return expand_tilde(env_socket)
  end

  return DEFAULT_SOCK
end

local function mkdir_p(dir_path)
  local ok, err = unix.makedirs(dir_path, tonumber("755", 8))
  if not ok then
    return nil, "failed to create directory " .. dir_path .. ": " .. tostring(err)
  end
  return true
end

local function file_exists(file_path)
  return unix.stat(file_path) ~= nil
end

local function read_file(file_path)
  local f = io.open(file_path, "r")
  if not f then
    return nil
  end
  local content = f:read("*all")
  f:close()
  return content
end

local function is_process_running(pid)
  local ok = unix.kill(pid, 0)
  return ok ~= nil
end

local function get_running_pid(pidfile)
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

local function wait_for_socket(socket_path, timeout)
  timeout = timeout or 5
  local sleep_interval_ms = 100
  local elapsed = 0

  while elapsed < timeout do
    if file_exists(socket_path) then
      local fd = unix.socket(unix.AF_UNIX, unix.SOCK_STREAM, 0)
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

local function load_zsh_environment()
  local env = {}

  local zsh_path = unix.commandv("zsh")
  if not zsh_path then
    return env
  end

  local spawn = require("cosmic.spawn").spawn
  local ok, output = spawn({zsh_path, "-l", "-c", "env -0"}):read()
  if not ok or not output then
    return env
  end

  local i = 1
  while i <= #output do
    local next_null = output:find("\0", i, true)
    if not next_null then
      break
    end
    local line = output:sub(i, next_null - 1)
    local key, value = line:match("^([^=]+)=(.*)$")
    if key then
      env[key] = value
    end
    i = next_null + 1
  end

  return env
end

local function setup_nvim_environment(nvim_bin)
  local env_table = load_zsh_environment()
  env_table["NVIM_SERVER_MODE"] = "1"
  env_table["WHEREAMI"] = whereami.get_with_emoji()
  set_vimruntime(env_table, nvim_bin)
  local env = {}
  for k, v in pairs(env_table) do
    table.insert(env, k .. "=" .. v)
  end
  return env
end

local function exec_nvim_server(sock, env, nvim_bin)
  os.remove(sock)
  unix.execve(nvim_bin, {"nvim", "--listen", sock, "--headless"}, env)
end

local function cmd_start(paths, nvim_bin)
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

  local lockfd
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

local function cmd_stop(paths)
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

local function cmd_status(paths)
  local pid = get_running_pid(paths.pid)
  if pid then
    io.write(string.format("nvim server is running (pid %d) at %s\n", pid, paths.sock))
    return 0
  else
    io.write(string.format("nvim server is not running at %s\n", paths.sock))
    return 1
  end
end

local function cmd_restart(paths, nvim_bin)
  cmd_stop(paths)
  unix.nanosleep(1, 0)
  return cmd_start(paths, nvim_bin)
end

local function cmd_cleanup(paths)
  os.remove(paths.sock)
  return 0
end

local commands = {
  start = cmd_start,
  stop = cmd_stop,
  status = cmd_status,
  restart = cmd_restart,
  cleanup = cmd_cleanup,
}

local function daemon_mode(args, nvim_bin)
  local cli_socket, remaining_args = parse_socket_option(args)
  local socket_path = get_socket_path(cli_socket)
  local paths = derive_paths(socket_path)

  local command = remaining_args[1] or ""
  local cmd_fn = commands[command]

  if cmd_fn then
    if command == "start" or command == "restart" then
      return cmd_fn(paths, nvim_bin)
    else
      return cmd_fn(paths)
    end
  else
    io.stderr:write("nvimd: unknown command: " .. command .. "\n")
    io.stderr:write("usage: nvimd [--socket PATH] {start|stop|status|restart|cleanup}\n")
    return 1
  end
end

local function has_remote_flag(args)
  for _, arg in ipairs(args) do
    if arg == "--server" or arg:match("^%-%-remote%-") then
      return true
    end
  end
  return false
end

local function is_socket_connectable(socket_path)
  if not file_exists(socket_path) then
    return false
  end

  local fd = unix.socket(unix.AF_UNIX, unix.SOCK_STREAM, 0)
  if not fd or fd < 0 then
    return false
  end

  local ok = unix.connect(fd, socket_path)
  unix.close(fd)
  return ok
end

local function ensure_server_running(paths, nvim_bin)
  if not is_socket_connectable(paths.sock) then
    cmd_start(paths, nvim_bin)
  end
end

local function client_mode(args, nvim_bin)
  local nvim_invim = os.getenv("NVIM_INVIM")
  local nvim_server_mode = os.getenv("NVIM_SERVER_MODE")
  local env = env_with_vimruntime(nvim_bin)

  if nvim_invim or nvim_server_mode then
    local new_args = {"nvim"}
    for _, arg in ipairs(args) do
      table.insert(new_args, arg)
    end
    unix.execve(nvim_bin, new_args, env)
    return
  end

  local cli_socket, remaining_args = parse_socket_option(args)
  local use_remote = has_remote_flag(remaining_args)

  if use_remote then
    local socket_path = get_socket_path(cli_socket)
    local paths = derive_paths(socket_path)

    ensure_server_running(paths, nvim_bin)

    local new_args = {"nvim"}
    local has_server_arg = false
    for _, arg in ipairs(remaining_args) do
      if arg == "--server" then
        has_server_arg = true
      end
    end

    if not has_server_arg then
      table.insert(new_args, "--server")
      table.insert(new_args, paths.sock)
    end

    for _, arg in ipairs(remaining_args) do
      table.insert(new_args, arg)
    end

    unix.execve(nvim_bin, new_args, env)
  else
    local new_args = {"nvim"}
    for _, arg in ipairs(remaining_args) do
      table.insert(new_args, arg)
    end
    unix.execve(nvim_bin, new_args, env)
  end
end

local function cleanup_and_exit(signum, paths)
  os.remove(paths.sock)
  if file_exists(paths.pid) then
    local pid = get_running_pid(paths.pid)
    if not pid then
      os.remove(paths.pid)
    end
  end
  os.exit(128 + signum)
end

local function main(args)
  local program_name = args[0]:match("([^/]+)$")
  local nvim_bin, err = resolve_nvim_bin()
  if not nvim_bin then
    io.stderr:write("error: " .. err .. "\n")
    return 1
  end

  local cmd_args = {}
  for i = 1, #args do
    cmd_args[i] = args[i]
  end

  local cli_socket, _ = parse_socket_option(cmd_args)
  local socket_path = get_socket_path(cli_socket)
  local paths = derive_paths(socket_path)

  unix.sigaction(unix.SIGINT, function() cleanup_and_exit(unix.SIGINT, paths) end)
  unix.sigaction(unix.SIGTERM, function() cleanup_and_exit(unix.SIGTERM, paths) end)

  if program_name == "nvimd" or program_name == "nvimd.lua" then
    return daemon_mode(cmd_args, nvim_bin)
  else
    return client_mode(cmd_args, nvim_bin)
  end
end

return {
  main = main,
  load_zsh_environment = load_zsh_environment,
  setup_nvim_environment = setup_nvim_environment,
  resolve_nvim_bin = resolve_nvim_bin,
}
