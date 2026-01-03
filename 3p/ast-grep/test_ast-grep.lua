#!/usr/bin/env run-test.lua
local lu = require("luaunit")
local spawn = require("cosmic.spawn")
local path = require("cosmo.path")

-- find ast-grep binary in staged deps
local function find_binary()
  for _, dep in ipairs(TEST_DEPS or {}) do
    if dep:match("/ast%-grep$") or dep:match("/sg$") then
      return dep
    end
  end
  -- fallback to staged path
  local staged = path.join(os.getenv("TEST_O"), "3p/ast-grep/version.lua.staged")
  return path.join(staged, "sg")
end

local bin = find_binary()

TestAstGrep = {}

function TestAstGrep:test_version()
  local handle = spawn({ bin, "--version" })
  lu.assertEquals(handle:wait(), 0)
end
