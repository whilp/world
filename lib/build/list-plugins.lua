#!/usr/bin/env lua
-- Lists plugin names from nvim-pack-lock.json
-- Usage: lua lib/build/list-plugins.lua
-- Outputs space-separated plugin names for make

local unix = require("cosmo.unix")

local PACK_LOCK = ".config/nvim/nvim-pack-lock.json"

local function read_file(filepath)
  local fd = unix.open(filepath, unix.O_RDONLY)
  if not fd then
    return nil, "failed to open " .. filepath
  end
  local chunks = {}
  while true do
    local chunk = unix.read(fd, 65536)
    if not chunk or chunk == "" then break end
    table.insert(chunks, chunk)
  end
  unix.close(fd)
  return table.concat(chunks)
end

local function list_plugins()
  local content, err = read_file(PACK_LOCK)
  if not content then
    io.stderr:write("error: " .. err .. "\n")
    os.exit(1)
  end

  local plugins = {}
  local in_plugins = false

  for line in content:gmatch("[^\n]+") do
    if line:match('^%s*"plugins":%s*{%s*$') then
      in_plugins = true
    elseif in_plugins then
      local name = line:match('^%s*"([^"]+)":%s*{%s*$')
      if name then
        table.insert(plugins, name)
      end
    end
  end

  io.write(table.concat(plugins, " "))
end

list_plugins()
