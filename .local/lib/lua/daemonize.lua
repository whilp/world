local ffi = require("ffi")
local unistd = require("posix.unistd")
local fcntl = require("posix.fcntl")
local stat = require("posix.sys.stat")
local pwd = require("posix.pwd")

ffi.cdef[[
  int setuid(unsigned int uid);
  int setgid(unsigned int gid);
  int initgroups(const char *user, unsigned int group);
]]

local M = {}

M.daemon = function(opts)
  opts = opts or {}

  local pid = unistd.fork()
  if pid == nil then
    return nil, "first fork failed"
  end
  if pid > 0 then
    unistd._exit(0)
  end

  if not unistd.setpid("s") then
    return nil, "setsid failed"
  end

  pid = unistd.fork()
  if pid == nil then
    return nil, "second fork failed"
  end
  if pid > 0 then
    unistd._exit(0)
  end

  stat.umask(0)

  if not opts.nochdir then
    unistd.chdir("/")
  end

  if not opts.noclose then
    local null_fd = fcntl.open("/dev/null", fcntl.O_RDWR)
    if null_fd then
      unistd.dup2(null_fd, 0)
      unistd.dup2(null_fd, 1)
      unistd.dup2(null_fd, 2)
      if null_fd > 2 then
        unistd.close(null_fd)
      end
    end
  end

  return true
end

M.write_pidfile = function(path)
  if not path or path == "" then
    return nil, "pid file path required"
  end

  local pid = unistd.getpid()
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

  local posix = require("posix")
  local fd, err = fcntl.open(path, fcntl.O_CREAT + fcntl.O_RDWR, 384)
  if not fd then
    return nil, "failed to open lock file: " .. tostring(err)
  end

  local content = unistd.read(fd, 64) or ""
  local existing_pid = tonumber(content:match("^(%d+)"))

  if existing_pid and existing_pid > 0 then
    local exists = posix.stat("/proc/" .. existing_pid) ~= nil
    if exists then
      unistd.close(fd)
      return nil, "another instance is running (pid " .. existing_pid .. ")"
    end
  end

  local lock = {
    l_type = fcntl.F_WRLCK,
    l_whence = fcntl.SEEK_SET,
    l_start = 0,
    l_len = 0,
  }

  local ok, err = fcntl.fcntl(fd, fcntl.F_SETLK, lock)
  if not ok then
    unistd.close(fd)
    return nil, "failed to acquire lock"
  end

  unistd.ftruncate(fd, 0)
  unistd.lseek(fd, 0, 0)
  local pid_str = tostring(unistd.getpid()) .. "\n"
  unistd.write(fd, pid_str, #pid_str)

  local flags = fcntl.fcntl(fd, fcntl.F_GETFD)
  if flags then
    local bit = require("bit")
    fcntl.fcntl(fd, fcntl.F_SETFD, bit.band(flags, bit.bnot(fcntl.FD_CLOEXEC)))
  end

  return fd
end

M.switch_user = function(user)
  if not user or user == "" then
    return nil, "user required"
  end

  if unistd.geteuid() ~= 0 then
    return nil, "must be root to switch user"
  end

  local pw, err = pwd.getpwnam(user)
  if not pw then
    return nil, "user not found: " .. tostring(err)
  end

  local uid = pw.pw_uid
  local gid = pw.pw_gid

  if ffi.C.initgroups(user, gid) ~= 0 then
    return nil, "initgroups failed"
  end

  if ffi.C.setgid(gid) ~= 0 then
    return nil, "setgid failed"
  end

  if ffi.C.setuid(uid) ~= 0 then
    return nil, "setuid failed"
  end

  return true
end

M.redirect_output = function(stdout_path, stderr_path, append)
  local flags = fcntl.O_CREAT + fcntl.O_WRONLY
  if append then
    flags = flags + fcntl.O_APPEND
  else
    flags = flags + fcntl.O_TRUNC
  end

  if stdout_path and stderr_path and stdout_path == stderr_path then
    local fd = fcntl.open(stdout_path, flags, 420)
    if not fd then
      return nil, "failed to open output file: " .. stdout_path
    end
    unistd.dup2(fd, 1)
    unistd.dup2(fd, 2)
    if fd > 2 then
      unistd.close(fd)
    end
  else
    if stdout_path then
      local fd = fcntl.open(stdout_path, flags, 420)
      if not fd then
        return nil, "failed to open stdout file: " .. stdout_path
      end
      unistd.dup2(fd, 1)
      if fd > 2 then
        unistd.close(fd)
      end
    end

    if stderr_path then
      local fd = fcntl.open(stderr_path, flags, 420)
      if not fd then
        return nil, "failed to open stderr file: " .. stderr_path
      end
      unistd.dup2(fd, 2)
      if fd > 2 then
        unistd.close(fd)
      end
    end
  end

  return true
end

M.setenv = function(name, value)
  if not name or name == "" then
    return nil, "environment variable name required"
  end

  local stdlib = require("posix.stdlib")
  local ok = stdlib.setenv(name, value or "")
  if not ok then
    return nil, "setenv failed"
  end

  return true
end

return M
