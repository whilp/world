#!/usr/bin/env lua
-- teal ignore: type annotations needed

local cosmo = require("cosmo")
local reporter = require("build.report")

local function main(check_dir)
  return reporter.report({
    pattern = "%.teal%.checked$",
    checker = "teal",
    check_dir = check_dir,
  })
end

if cosmo.is_main() then
  os.exit(main(...))
end
