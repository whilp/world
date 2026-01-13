-- daemonize: process daemonization utilities
local unix = require("cosmo.unix")

local record M
  write_pidfile: function(path: string): boolean, string
  acquire_lock: function(path: string): number, string
  redirect_output: function(stdout_path: string, stderr_path: string, append: boolean): boolean, string
end

function M.write_pidfile(path: string): boolean, string
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

function M.acquire_lock(path: string): number, string
  if not path or path == "" then
    return nil, "lock file path required"
  end

  local fd, err = unix.open(path, unix.O_CREAT + unix.O_RDWR, tonumber("0600", 8) as number)
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

  local ok = unix.fcntl(fd, unix.F_SETLK, unix.F_WRLCK, unix.SEEK_SET, 0, 0)
  if not ok then
    unix.close(fd)
    return nil, "failed to acquire lock"
  end

  unix.ftruncate(fd, 0)
  unix.lseek(fd, 0, 0)
  local pid_str = tostring(unix.getpid()) .. "\n"
  unix.write(fd, pid_str)

  return fd
end

function M.redirect_output(stdout_path: string, stderr_path: string, append: boolean): boolean, string
  local flags = unix.O_CREAT + unix.O_WRONLY
  if append then
    flags = flags + unix.O_APPEND
  else
    flags = flags + unix.O_TRUNC
  end

  if stdout_path and stderr_path and stdout_path == stderr_path then
    local fd = unix.open(stdout_path, flags, tonumber("0644", 8) as number)
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
      local fd = unix.open(stdout_path, flags, tonumber("0644", 8) as number)
      if not fd then
        return nil, "failed to open stdout file: " .. stdout_path
      end
      unix.dup(fd, 1)
      if fd > 2 then
        unix.close(fd)
      end
    end

    if stderr_path then
      local fd = unix.open(stderr_path, flags, tonumber("0644", 8) as number)
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

return M
