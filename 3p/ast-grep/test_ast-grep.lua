#!/usr/bin/env run-test.lua
local lu = require("luaunit")
local spawn = require("cosmic.spawn")
local path = require("cosmo.path")

local function find_staged(pattern)
  for _, dep in ipairs(TEST_DEPS or {}) do
    if dep:match(pattern) and dep:match("%.staged$") then
      return dep
    end
  end
end

local staged = find_staged("ast%-grep")
local bin = path.join(staged, "sg")

TestAstGrep = {}

function TestAstGrep:test_version()
  local handle = spawn({ bin, "--version" })
  lu.assertEquals(handle:wait(), 0)
end
