#!/usr/bin/env lua
-- Minimal test to reproduce APE lua unix.open crash on linux-x86_64
local unix = require("cosmo.unix")

local function test_unix_open_write(path, size)
  io.stderr:write("Testing unix.open write to: " .. path .. " (size: " .. size .. ")\n")
  io.stderr:flush()

  -- Create parent directory
  local dir = path:match("(.*/)")
  if dir then
    unix.makedirs(dir)
  end

  io.stderr:write("  opening for write...\n")
  io.stderr:flush()

  local fd = unix.open(path, unix.O_WRONLY | unix.O_CREAT | unix.O_TRUNC, tonumber("755", 8))
  if not fd then
    io.stderr:write("  FAILED to open\n")
    return false
  end

  io.stderr:write("  writing " .. size .. " bytes...\n")
  io.stderr:flush()

  -- Write data in chunks
  local chunk_size = 65536
  local written = 0
  while written < size do
    local to_write = math.min(chunk_size, size - written)
    local data = string.rep("X", to_write)
    unix.write(fd, data)
    written = written + to_write
  end

  io.stderr:write("  closing...\n")
  io.stderr:flush()

  unix.close(fd)

  io.stderr:write("  SUCCESS\n")
  io.stderr:flush()

  -- Clean up
  unix.unlink(path)
  return true
end

-- Test with various sizes (the cosmos lua binary is ~5.8MB)
local test_dir = "/tmp/test_unix_open_" .. os.time()
local sizes = {1024, 65536, 1024*1024, 5*1024*1024}

for _, size in ipairs(sizes) do
  local path = test_dir .. "/test_" .. size .. ".bin"
  if not test_unix_open_write(path, size) then
    os.exit(1)
  end
end

-- Clean up test directory
unix.rmrf(test_dir)

io.stderr:write("\nAll tests passed!\n")
