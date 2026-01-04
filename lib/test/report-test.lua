#!/usr/bin/env lua
-- teal ignore: type annotations needed

local cosmo = require("cosmo")
local walk = require("cosmic.walk")
local path = require("cosmo.path")
local common = require("checker.common")

local function main(test_dir)
  test_dir = test_dir or "o"

  local results = {
    pass = {},
    fail = {},
    skip = {},
    ignore = {},
  }
  local all_results = {}

  -- find all .tested files
  local tested_files = walk.collect(test_dir, "%.tested$")
  table.sort(tested_files)
  for _, file in ipairs(tested_files) do
    local content = cosmo.Slurp(file)
    if content then
      local result = common.parse_result(content)
      if result then
        local test_name = file:gsub("^o/", ""):gsub("%.tested$", "")
        result.name = test_name
        result.file = file
        table.insert(all_results, result)

        local status = result.status
        if status == "pass" then
          table.insert(results.pass, result)
        elseif status == "fail" then
          table.insert(results.fail, result)
        elseif status == "skip" then
          table.insert(results.skip, result)
        elseif status == "ignore" then
          table.insert(results.ignore, result)
        end
      end
    end
  end

  -- print each test result with padded status
  local status_icons = common.status_icons()
  for _, result in ipairs(all_results) do
    local status = string.upper(result.status)
    local icon = status_icons[result.status] or " "
    local padded = string.format("%-6s", status)
    local line = icon .. " " .. padded .. " " .. result.name
    if result.status == "skip" and result.message then
      line = line .. " (" .. result.message .. ")"
    end
    print(line)
  end

  -- print summary
  local total = #results.pass + #results.fail + #results.skip + #results.ignore
  print(string.format(
    "%d tests: %d passed, %d failed, %d skipped, %d ignored",
    total,
    #results.pass,
    #results.fail,
    #results.skip,
    #results.ignore
  ))

  -- print failed tests with output
  if #results.fail > 0 then
    print("")
    print("FAILURES:")
    for _, result in ipairs(results.fail) do
      print("")
      print(string.format("--- %s ---", result.name))
      if result.message then
        print(result.message)
      end
      if result.stdout and result.stdout ~= "" then
        print("")
        print("stdout:")
        print(result.stdout)
      end
      if result.stderr and result.stderr ~= "" then
        print("")
        print("stderr:")
        print(result.stderr)
      end
    end
  end

  return #results.fail > 0 and 1 or 0
end

if cosmo.is_main() then
  os.exit(main(...))
end
