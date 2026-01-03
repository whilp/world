#!/usr/bin/env lua

local cosmo = require("cosmo")
local walk = require("cosmic.walk")

local function parse_result(content)
  local result = {}

  local first_line = content:match("^([^\n]+)")
  if not first_line then
    return nil
  end

  local status, message = first_line:match("^(%w+):?%s*(.*)")
  result.status = status or first_line
  result.message = message ~= "" and message or nil

  result.stdout = content:match("## stdout\n\n(.-)\n## stderr") or ""
  result.stderr = content:match("## stderr\n\n(.*)$") or ""

  return result
end

local function main(check_dir)
  check_dir = check_dir or "o"

  local results = {
    pass = {},
    fail = {},
    skip = {},
    ignore = {},
  }
  local all_results = {}

  local checked_files = walk.collect(check_dir, "%.astgrep%.checked$")
  table.sort(checked_files)
  for _, file in ipairs(checked_files) do
    local content = cosmo.Slurp(file)
    if content then
      local result = parse_result(content)
      if result then
        local name = file:gsub("^o/", ""):gsub("%.astgrep%.checked$", "")
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

  for _, result in ipairs(all_results) do
    if result.status ~= "ignore" then
      local status = string.upper(result.status)
      local padded = string.format("%-6s", status)
      print(padded .. " " .. result.name)
    end
  end

  local total = #results.pass + #results.fail + #results.skip
  print(string.format(
    "%d files: %d passed, %d failed, %d skipped, %d ignored",
    total + #results.ignore,
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
