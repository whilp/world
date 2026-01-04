#!/usr/bin/env lua

local cosmo = require("cosmo")
local walk = require("cosmic.walk")
local common = require("checker.common")

local function main(output_dir)
  output_dir = output_dir or "o"

  local results = {
    pass = {},
    skip = {},
    ignore = {},
    fail = {},
  }
  local all_results = {}

  local updated_files = walk.collect(output_dir, "%.updated$")
  table.sort(updated_files)

  for _, file in ipairs(updated_files) do
    local content = cosmo.Slurp(file)
    if content then
      local result = common.parse_result(content)
      if result then
        local name = file:gsub("^o/", ""):gsub("%.updated$", "")
        result.name = name
        result.file = file
        table.insert(all_results, result)

        local status = result.status
        if results[status] then
          table.insert(results[status], result)
        end
      end
    end
  end

  local status_icons = common.status_icons()
  for _, result in ipairs(all_results) do
    local status = string.upper(result.status)
    local icon = status_icons[result.status] or " "
    local padded = string.format("%-6s", status)
    local line = icon .. " " .. padded .. " " .. result.name
    if result.message then
      if result.status == "skip" then
        line = line .. " (updated: " .. result.message .. ")"
      else
        line = line .. " (" .. result.message .. ")"
      end
    elseif result.status == "skip" then
      line = line .. " (updated)"
    end
    print(line)
  end

  local total = #results.pass + #results.skip + #results.ignore + #results.fail
  local updates_available = #results.skip

  print(string.format(
    "%d checked, %d updates available",
    total,
    updates_available
  ))

  return 0
end

if cosmo.is_main() then
  os.exit(main(...))
end
