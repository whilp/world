#!/usr/bin/env run-test.lua
local spawn = require("cosmic.spawn")
local path = require("cosmo.path")

local record SpawnHandle
  wait: function(self: SpawnHandle): number
end

global TEST_BIN_DIR: string

local lua_dist = path.join((os.getenv("TEST_BIN_DIR") as string):gsub("/tl$", "/lua"), "bin", "lua.dist")
local tl_bin = path.join(os.getenv("TEST_BIN_DIR") as string, "bin", "tl")

local function test_version()
  local handle = spawn({ lua_dist, tl_bin, "--version" }) as SpawnHandle
  local exit_code = handle:wait()
  assert(exit_code == 0, "expected exit code 0, got " .. tostring(exit_code))
end
test_version()
