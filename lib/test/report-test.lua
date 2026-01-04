#!/usr/bin/env lua
-- teal ignore: type annotations needed

local cosmo = require("cosmo")
local reporter = require("build.report")

local function main(test_dir)
  return reporter.report({
    pattern = "%.tested$",
    check_dir = test_dir or "o",
    strip_suffix = "%.tested$",
    name_transform = function(name)
      return name:gsub("^o/", "")
    end,
    summary_format = function(results)
      local total = #results.pass + #results.fail + #results.skip + #results.ignore
      return string.format(
        "%d tests: %d passed, %d failed, %d skipped, %d ignored",
        total,
        #results.pass,
        #results.fail,
        #results.skip,
        #results.ignore
      )
    end,
  })
end

if cosmo.is_main() then
  os.exit(main(...))
end
