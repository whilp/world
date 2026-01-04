#!/usr/bin/env lua

local cosmo = require("cosmo")
local reporter = require("build.report")

local function main(...)
  local files = {...}
  if #files == 0 then
    io.stderr:write("usage: report-update.lua <file.updated> ...\n")
    return 1
  end

  return reporter.report({
    files = files,
    strip_suffix = "%.updated$",
    name_transform = function(name)
      return name:gsub("^o/", "")
    end,
    summary_format = function(results)
      local total = #results.pass + #results.skip + #results.ignore + #results.fail
      local updates_available = #results.skip
      return string.format(
        "%d checked, %d updates available",
        total,
        updates_available
      )
    end,
  })
end

if cosmo.is_main() then
  os.exit(main(...))
end
