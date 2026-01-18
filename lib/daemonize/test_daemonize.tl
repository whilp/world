local unix = require("cosmo.unix")
local path = require("cosmo.path")

local daemonize = require('daemonize')

global TEST_TMPDIR: string

local function test_acquire_lock()
  local lock_path = path.join(TEST_TMPDIR, "lock")

  local fd, err = daemonize.acquire_lock(lock_path)
  assert(fd, "acquire_lock should return a file descriptor: " .. tostring(err))

  if fd then
    unix.close(fd)
  end

  unix.unlink(lock_path)
end
test_acquire_lock()

local function test_write_pidfile()
  local pid_path = path.join(TEST_TMPDIR, "pidfile")

  local ok, err = daemonize.write_pidfile(pid_path)
  assert(ok, "write_pidfile should succeed: " .. tostring(err))

  local f = io.open(pid_path, "r")
  assert(f, "pidfile should exist")
  local content = f:read("*a")
  f:close()

  local pid = tonumber((content:match("^(%d+)")))
  assert(pid, "pidfile should contain a number")
  assert(pid == unix.getpid(), "pidfile should contain current process pid")

  unix.unlink(pid_path)
end
test_write_pidfile()

local function test_write_pidfile_requires_path()
  local ok, err = daemonize.write_pidfile("")
  assert(ok == nil, "write_pidfile should fail with empty path")
  assert(err, "write_pidfile should return error message")
  assert(err:find("required"), "error should mention path is required")
end
test_write_pidfile_requires_path()

local function test_acquire_lock_requires_path()
  local fd, err = daemonize.acquire_lock("")
  assert(fd == nil, "acquire_lock should fail with empty path")
  assert(err, "acquire_lock should return error message")
  assert(err:find("required"), "error should mention path is required")
end
test_acquire_lock_requires_path()
