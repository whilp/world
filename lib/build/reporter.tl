#!/usr/bin/env lua

local cosmo = require("cosmo")
local getopt = require("cosmo.getopt")
local common = require("checker.common")

local record CheckResult
  status: string
  message: string
  name: string
  file: string
  checker: string
end

local record CategorizedResults
  pass: {CheckResult}
  fail: {CheckResult}
  skip: {CheckResult}
  ignore: {CheckResult}
end

local function extract_checker(filename: string): string
  local checker = filename:match("%.([^%.]+)%.ok$")
  return checker
end

local function main(...: string): number, string
  local args = {...}
  local dir: string = nil

  local longopts = {{"dir", "required"}}
  local parser = getopt.new(args, "", longopts)

  while true do
    local opt, optarg = parser:next()
    if not opt then break end
    if opt == "dir" then
      dir = optarg
    elseif opt == "?" then
      return 1, "usage: reporter.lua --dir DIR FILES..."
    end
  end

  local files = parser:remaining()
  if not files or #files == 0 then
    return 1, "usage: reporter.lua --dir DIR FILES..."
  end
  if not dir then
    return 1, "error: --dir is required"
  end

  local function name_transform(file_name: string): string
    if file_name:sub(1, #dir) == dir and file_name:sub(#dir + 1, #dir + 1) == "/" then
      return file_name:sub(#dir + 2)
    end
    return file_name
  end

  -- group files by checker type
  local checker_files: {string:{string}} = {}
  for _, file in ipairs(files) do
    local checker = extract_checker(file)
    if checker then
      checker_files[checker] = checker_files[checker] or {}
      table.insert(checker_files[checker], file)
    end
  end

  -- get sorted list of checkers
  local checkers: {string} = {}
  for checker in pairs(checker_files) do
    table.insert(checkers, checker)
  end
  table.sort(checkers)

  -- collect results for each checker
  local checker_results: {string:CategorizedResults} = {}
  local all_results: {CheckResult} = {}

  for _, checker in ipairs(checkers) do
    local results: CategorizedResults = {pass = {}, fail = {}, skip = {}, ignore = {}}
    local strip_suffix = "%." .. checker:gsub("%-", "%%-") .. "%.ok$"
    local show_checker = checker ~= "update"

    local result_files = checker_files[checker]
    table.sort(result_files)

    for _, file in ipairs(result_files) do
      local content = cosmo.Slurp(file)
      if content then
        local parsed = common.parse_result(content)
        if parsed then
          local result: CheckResult = {
            status = parsed.status,
            message = parsed.message,
            name = name_transform(common.strip_prefix(file):gsub(strip_suffix, "")),
            file = file,
            checker = show_checker and checker or nil,
          }
          local status_list = (results as {string:{CheckResult}})[result.status]
          if status_list then
            table.insert(status_list, result)
          end
          table.insert(all_results, result)
        end
      end
    end

    checker_results[checker] = results
  end

  -- sort all results by name, then checker
  table.sort(all_results, function(a: CheckResult, b: CheckResult): boolean
    if a.name ~= b.name then return a.name < b.name end
    return (a.checker or "") < (b.checker or "")
  end)

  -- build output
  local parts: {string} = {}
  local status_icons = common.status_icons()
  table.insert(parts, common.format_results(all_results as {common.CheckResult}, status_icons))

  -- per-checker summaries
  local total_passed, total_failed, total_skipped, total_ignored = 0, 0, 0, 0

  for _, checker in ipairs(checkers) do
    local results = checker_results[checker]
    local summary = common.format_summary(checker, results as common.CategorizedResults)
    if summary and summary ~= "" then
      table.insert(parts, summary)
    end
    total_passed = total_passed + #results.pass
    total_failed = total_failed + #results.fail
    total_skipped = total_skipped + #results.skip
    total_ignored = total_ignored + #results.ignore
  end

  -- total summary (only for multiple checkers)
  if #checkers > 1 then
    local total = total_passed + total_failed + total_skipped + total_ignored
    table.insert(parts, "")
    table.insert(parts, string.format(
      "total: %d checks: %d passed, %d failed, %d skipped, %d ignored",
      total, total_passed, total_failed, total_skipped, total_ignored
    ))
  end

  -- failures
  local failures = common.format_failures(all_results as {common.CheckResult})
  if failures then
    table.insert(parts, failures)
  end

  print(table.concat(parts, "\n"))
  return total_failed == 0 and 0 or 1, nil
end

if cosmo.is_main() then
  local code, err = main(...)
  if err then
    io.stderr:write(err .. "\n")
  end
  os.exit(code)
end
