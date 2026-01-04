#!/usr/bin/env lua
-- teal ignore: type annotations needed

local cosmo = require("cosmo")
local reporter = require("reporter.report")

local function main(check_dir)
  return reporter.report({
    pattern = "%.astgrep%.checked$",
    checker = "ast-grep",
    check_dir = check_dir,
  })
end

if cosmo.is_main() then
  os.exit(main(...))
end
