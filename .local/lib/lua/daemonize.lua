local cosmo = require("cosmo")
local unix = cosmo.unix

local M = {}

M.daemon = function(opts)
  opts = opts or {}

  local pid = unix.fork()
  if pid == nil then
    return nil, "first fork failed"
  end
  if pid > 0 then
    unix.exit(0)
  end

  if not unix.setsid() then
    return nil, "setsid failed"
  end

  pid = unix.fork()
  if pid == nil then
    return nil, "second fork failed"
  end
  if pid > 0 then
    unix.exit(0)
  end

  unix.umask(0)

  if not opts.nochdir then
    unix.chdir("/")
  end

  if not opts.noclose then
    local null_fd = unix.open("/dev/null", unix.O_RDWR)
    if null_fd then
      unix.dup(null_fd, 0)
      unix.dup(null_fd, 1)
      unix.dup(null_fd, 2)
      if null_fd > 2 then
        unix.close(null_fd)
      end
    end
  end

  return true
end

M.write_pidfile = function(path)
  if not path or path == "" then
    return nil, "pid file path required"
  end

  local pid = unix.getpid()
  local f, err = io.open(path, "w")
  if not f then
    return nil, "failed to open pid file: " .. tostring(err)
  end

  f:write(tostring(pid) .. "\n")
  f:close()
  return true
end

M.acquire_lock = function(path)
  if not path or path == "" then
    return nil, "lock file path required"
  end

  local fd, err = unix.open(path, unix.O_CREAT + unix.O_RDWR, 384)
  if not fd then
    return nil, "failed to open lock file: " .. tostring(err)
  end

  local content = unix.read(fd, 64) or ""
  local existing_pid = tonumber(content:match("^(%d+)"))

  if existing_pid and existing_pid > 0 then
    local exists = unix.stat("/proc/" .. existing_pid) ~= nil
    if exists then
      unix.close(fd)
      return nil, "another instance is running (pid " .. existing_pid .. ")"
    end
  end

  local lock = {
    l_type = unix.F_WRLCK,
    l_whence = unix.SEEK_SET,
    l_start = 0,
    l_len = 0,
  }

  local ok, err = unix.fcntl(fd, unix.F_SETLK, lock)
  if not ok then
    unix.close(fd)
    return nil, "failed to acquire lock"
  end

  unix.ftruncate(fd, 0)
  unix.lseek(fd, 0, 0)
  local pid_str = tostring(unix.getpid()) .. "\n"
  unix.write(fd, pid_str)

  local flags = unix.fcntl(fd, unix.F_GETFD)
  if flags then
    local bit = require("bit")
    unix.fcntl(fd, unix.F_SETFD, bit.band(flags, bit.bnot(unix.FD_CLOEXEC)))
  end

  return fd
end

M.redirect_output = function(stdout_path, stderr_path, append)
  local flags = unix.O_CREAT + unix.O_WRONLY
  if append then
    flags = flags + unix.O_APPEND
  else
    flags = flags + unix.O_TRUNC
  end

  if stdout_path and stderr_path and stdout_path == stderr_path then
    local fd = unix.open(stdout_path, flags, 420)
    if not fd then
      return nil, "failed to open output file: " .. stdout_path
    end
    unix.dup(fd, 1)
    unix.dup(fd, 2)
    if fd > 2 then
      unix.close(fd)
    end
  else
    if stdout_path then
      local fd = unix.open(stdout_path, flags, 420)
      if not fd then
        return nil, "failed to open stdout file: " .. stdout_path
      end
      unix.dup(fd, 1)
      if fd > 2 then
        unix.close(fd)
      end
    end

    if stderr_path then
      local fd = unix.open(stderr_path, flags, 420)
      if not fd then
        return nil, "failed to open stderr file: " .. stderr_path
      end
      unix.dup(fd, 2)
      if fd > 2 then
        unix.close(fd)
      end
    end
  end

  return true
end

M.setenv = function(name, value)
  if not name or name == "" then
    return nil, "environment variable name required"
  end

  local ok = unix.setenv(name, value or "", 1)
  if not ok then
    return nil, "setenv failed"
  end

  return true
end

return M
