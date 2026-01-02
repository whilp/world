#!/usr/bin/env lua
-- Lists plugin names from nvim-pack-lock.json
-- Outputs space-separated plugin names for make

local cosmo = require("cosmo")

local PACK_LOCK = ".config/nvim/nvim-pack-lock.json"

local f = io.open(PACK_LOCK, "r")
if not f then
  io.stderr:write("error: failed to open " .. PACK_LOCK .. "\n")
  os.exit(1)
end

local content = f:read("*a")
f:close()

local data = cosmo.DecodeJson(content)
if not data or not data.plugins then
  io.stderr:write("error: invalid pack-lock format\n")
  os.exit(1)
end

local plugins = {}
for name, _ in pairs(data.plugins) do
  table.insert(plugins, name)
end
table.sort(plugins)

io.write(table.concat(plugins, " "))
