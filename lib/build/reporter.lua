#!/usr/bin/env lua
-- teal ignore: type annotations needed

local cosmo = require("cosmo")
local getopt = require("cosmo.getopt")
local report_lib = require("build.report")

local function main(...)
  local args = {...}
  local dir = nil

  local longopts = {
    {"dir", "required"},
  }

  local parser = getopt.new(args, "", longopts)

  while true do
    local opt, optarg = parser:next()
    if not opt then
      break
    end

    if opt == "dir" then
      dir = optarg
    elseif opt == "?" then
      io.stderr:write("usage: reporter.lua [--dir DIR] NAME FILES...\n")
      return 1
    end
  end

  local remaining = parser:remaining()

  if not remaining or #remaining < 1 then
    io.stderr:write("usage: reporter.lua [--dir DIR] NAME FILES...\n")
    return 1
  end

  local name = remaining[1]
  local files = {}
  for i = 2, #remaining do
    table.insert(files, remaining[i])
  end

  if #files == 0 then
    io.stderr:write("error: no files specified\n")
    io.stderr:write("usage: reporter.lua [--dir DIR] NAME FILES...\n")
    return 1
  end

  dir = dir or os.getenv("TEST_O") or "o"
  local suffix_pattern = "%." .. name:gsub("%-", "%%-") .. "%.ok$"

  local config = {
    files = files,
    strip_suffix = suffix_pattern,
    checker = name,
    name_transform = function(file_name)
      local stripped = file_name
      if stripped:sub(1, #dir) == dir and stripped:sub(#dir + 1, #dir + 1) == "/" then
        stripped = stripped:sub(#dir + 2)
      end
      return stripped
    end,
  }

  if name == "test" then
    config.summary_format = function(results)
      local total = #results.pass + #results.fail + #results.skip + #results.ignore
      return string.format(
        "%d tests: %d passed, %d failed, %d skipped, %d ignored",
        total,
        #results.pass,
        #results.fail,
        #results.skip,
        #results.ignore
      )
    end
  elseif name == "update" then
    config.checker = nil
    config.summary_format = function(results)
      local total = #results.pass + #results.skip + #results.ignore + #results.fail
      local updates_available = #results.skip
      return string.format(
        "%d checked, %d updates available",
        total,
        updates_available
      )
    end
  end

  return report_lib.report(config)
end

if cosmo.is_main() then
  os.exit(main(...))
end
