local lu = require('luaunit')
local unix = require("cosmo.unix")

local daemonize = require('daemonize')

function test_acquire_lock()
  local lock_path = "/tmp/test_daemonize_lock"
  os.remove(lock_path)

  local fd, err = daemonize.acquire_lock(lock_path)
  lu.assertNotNil(fd, "acquire_lock should return a file descriptor: " .. tostring(err))

  if fd then
    unix.close(fd)
  end

  os.remove(lock_path)
end

function test_write_pidfile()
  local pid_path = "/tmp/test_daemonize_pidfile"
  os.remove(pid_path)

  local ok, err = daemonize.write_pidfile(pid_path)
  lu.assertNotNil(ok, "write_pidfile should succeed: " .. tostring(err))

  local f = io.open(pid_path, "r")
  lu.assertNotNil(f, "pidfile should exist")
  local content = f:read("*a")
  f:close()

  local pid = tonumber(content:match("^(%d+)"))
  lu.assertNotNil(pid, "pidfile should contain a number")
  lu.assertEquals(pid, unix.getpid(), "pidfile should contain current process pid")

  os.remove(pid_path)
end

function test_write_pidfile_requires_path()
  local ok, err = daemonize.write_pidfile("")
  lu.assertNil(ok, "write_pidfile should fail with empty path")
  lu.assertNotNil(err, "write_pidfile should return error message")
  lu.assertStrContains(err, "required", "error should mention path is required")
end

function test_acquire_lock_requires_path()
  local fd, err = daemonize.acquire_lock("")
  lu.assertNil(fd, "acquire_lock should fail with empty path")
  lu.assertNotNil(err, "acquire_lock should return error message")
  lu.assertStrContains(err, "required", "error should mention path is required")
end

os.exit(lu.LuaUnit.run())
