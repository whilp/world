#!/usr/bin/env lua
-- Test that mimics exactly what install.lua does for cosmos binaries
local unix = require("cosmo.unix")
local path = require("cosmo.path")

local function copy_file(src, dst)
  io.stderr:write("copy: " .. src .. " -> " .. dst .. "\n")
  io.stderr:flush()

  -- Get source permissions
  local st = unix.stat(src)
  local mode = st and st:mode() or tonumber("755", 8)

  io.stderr:write("  stat: mode=" .. string.format("%o", mode) .. "\n")
  io.stderr:flush()

  -- Open source
  io.stderr:write("  opening source...\n")
  io.stderr:flush()

  local fd_in = unix.open(src, unix.O_RDONLY)
  if not fd_in then
    io.stderr:write("  FAILED to open source\n")
    return false
  end

  io.stderr:write("  opened source (fd=" .. tostring(fd_in) .. ")\n")
  io.stderr:flush()

  -- Create parent directory
  local dir = path.dirname(dst)
  unix.makedirs(dir)

  -- Open destination
  io.stderr:write("  opening dest...\n")
  io.stderr:flush()

  local fd_out = unix.open(dst, unix.O_WRONLY | unix.O_CREAT | unix.O_TRUNC, mode)
  if not fd_out then
    unix.close(fd_in)
    io.stderr:write("  FAILED to open dest\n")
    return false
  end

  io.stderr:write("  opened dest (fd=" .. tostring(fd_out) .. ")\n")
  io.stderr:flush()

  -- Copy in chunks
  local bytes = 0
  while true do
    local chunk = unix.read(fd_in, 65536)
    if not chunk or #chunk == 0 then break end
    unix.write(fd_out, chunk)
    bytes = bytes + #chunk
  end

  io.stderr:write("  copied " .. bytes .. " bytes\n")
  io.stderr:flush()

  unix.close(fd_in)
  unix.close(fd_out)

  io.stderr:write("  SUCCESS\n")
  io.stderr:flush()

  return true
end

-- Test copying the actual cosmos lua binary
local src = "o/linux-x86_64/cosmos/staging/lua"
local dst = "/tmp/test_cosmos_copy/lua"

-- Check if source exists (CI extracts cosmos before this test runs)
local st = unix.stat(src)
if st then
  if copy_file(src, dst) then
    -- Verify the copy
    local st_dst = unix.stat(dst)
    io.stderr:write("\nVerification: dst size = " .. (st_dst and st_dst:size() or "N/A") .. "\n")
    unix.unlink(dst)
    unix.rmrf("/tmp/test_cosmos_copy")
  else
    os.exit(1)
  end
else
  io.stderr:write("Cannot find source file: " .. src .. "\n")
  os.exit(1)
end
