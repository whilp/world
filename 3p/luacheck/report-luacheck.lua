#!/usr/bin/env lua
-- teal ignore: type annotations needed

local cosmo = require("cosmo")
local walk = require("cosmic.walk")
local common = require("checker.common")

local function main(check_dir)
  check_dir = check_dir or os.getenv("TEST_O") or "o"

  local results = {
    pass = {},
    fail = {},
    skip = {},
    ignore = {},
  }
  local all_results = {}

  local checked_files = walk.collect(check_dir, "%.luacheck%.checked$")
  table.sort(checked_files)
  for _, file in ipairs(checked_files) do
    local content = cosmo.Slurp(file)
    if content then
      local result = common.parse_result(content)
      if result then
        local name = common.strip_prefix(file):gsub("%.luacheck%.checked$", "")
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
    if result.status ~= "pass" then
      if result.message then
        line = line .. " (luacheck: " .. result.message .. ")"
      else
        line = line .. " (luacheck)"
      end
    end
    print(line)
  end

  local total = #results.pass + #results.fail + #results.skip + #results.ignore
  print(string.format(
    "luacheck: %d checks: %d passed, %d failed, %d skipped, %d ignored",
    total,
    #results.pass,
    #results.fail,
    #results.skip,
    #results.ignore
  ))

  if #results.fail > 0 then
    print("")
    print("FAILURES:")
    for _, result in ipairs(results.fail) do
      print("")
      print(string.format("--- %s ---", result.name))
      if result.message then
        print(result.message)
      end
      if result.stderr and result.stderr ~= "" then
        print("")
        print(result.stderr)
      end
    end
  end

  return #results.fail > 0 and 1 or 0
end

if cosmo.is_main() then
  os.exit(main(...))
end
