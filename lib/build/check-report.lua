#!/usr/bin/env lua
-- teal ignore: type annotations needed

local cosmo = require("cosmo")
local walk = require("cosmic.walk")
local common = require("checker.common")

local function main(check_dir)
  check_dir = check_dir or os.getenv("TEST_O") or "o"

  local checkers = {
    { name = "ast-grep", pattern = "%.astgrep%.checked$" },
    { name = "luacheck", pattern = "%.luacheck%.checked$" },
    { name = "teal", pattern = "%.teal%.checked$" },
  }

  local all_results = {}
  local checker_results = {}

  for _, checker in ipairs(checkers) do
    local results = {
      pass = {},
      fail = {},
      skip = {},
      ignore = {},
    }

    local checked_files = walk.collect(check_dir, checker.pattern)
    table.sort(checked_files)
    for _, file in ipairs(checked_files) do
      local content = cosmo.Slurp(file)
      if content then
        local result = common.parse_result(content)
        if result then
          local name = common.strip_prefix(file):gsub(checker.pattern, "")
          result.name = name
          result.file = file
          result.checker = checker.name
          table.insert(all_results, result)

          local status = result.status
          if results[status] then
            table.insert(results[status], result)
          end
        end
      end
    end

    checker_results[checker.name] = results
  end

  table.sort(all_results, function(a, b)
    if a.name ~= b.name then
      return a.name < b.name
    end
    return a.checker < b.checker
  end)

  local status_icons = common.status_icons()

  for _, result in ipairs(all_results) do
    local status = string.upper(result.status)
    local icon = status_icons[result.status] or " "
    local padded = string.format("%-6s", status)
    local line = icon .. " " .. padded .. " " .. result.name
    if result.status ~= "pass" then
      if result.message then
        line = line .. " (" .. result.checker .. ": " .. result.message .. ")"
      else
        line = line .. " (" .. result.checker .. ")"
      end
    end
    print(line)
  end

  local total_checks = 0
  local total_passed = 0
  local total_failed = 0
  local total_skipped = 0
  local total_ignored = 0
  local has_failures = false

  for _, checker in ipairs(checkers) do
    local results = checker_results[checker.name]
    local total = #results.pass + #results.fail + #results.skip + #results.ignore
    if total > 0 then
      total_checks = total_checks + total
      total_passed = total_passed + #results.pass
      total_failed = total_failed + #results.fail
      total_skipped = total_skipped + #results.skip
      total_ignored = total_ignored + #results.ignore

      print(string.format(
        "%s: %d checks: %d passed, %d failed, %d skipped, %d ignored",
        checker.name,
        total,
        #results.pass,
        #results.fail,
        #results.skip,
        #results.ignore
      ))

      if #results.fail > 0 then
        has_failures = true
      end
    end
  end

  if total_checks > 0 then
    print("")
    print(string.format(
      "total: %d checks: %d passed, %d failed, %d skipped, %d ignored",
      total_checks,
      total_passed,
      total_failed,
      total_skipped,
      total_ignored
    ))
  end

  if has_failures then
    print("")
    print("FAILURES:")
    for _, result in ipairs(all_results) do
      if result.status == "fail" then
        print("")
        print(string.format("--- %s (%s) ---", result.name, result.checker))
        if result.message then
          print(result.message)
        end
        if result.stderr and result.stderr ~= "" then
          print("")
          print(result.stderr)
        end
      end
    end
  end

  return total_failed > 0 and 1 or 0
end

if cosmo.is_main() then
  os.exit(main(...))
end
