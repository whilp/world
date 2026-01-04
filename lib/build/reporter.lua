#!/usr/bin/env lua
-- teal ignore: type annotations needed

local cosmo = require("cosmo")
local getopt = require("cosmo.getopt")
local walk = require("cosmic.walk")
local common = require("checker.common")

local function collect_results(config)
  local check_dir = config.check_dir or os.getenv("TEST_O") or "o"
  local pattern = config.pattern
  local checker_name = config.checker
  local strip_suffix = config.strip_suffix or pattern

  local results = {
    pass = {},
    fail = {},
    skip = {},
    ignore = {},
  }

  local result_files
  if config.files then
    result_files = config.files
    table.sort(result_files)
  else
    result_files = walk.collect(check_dir, pattern)
    table.sort(result_files)
  end

  for _, file in ipairs(result_files) do
    local content = cosmo.Slurp(file)
    if content then
      local result = common.parse_result(content)
      if result then
        local name = common.strip_prefix(file):gsub(strip_suffix, "")
        if config.name_transform then
          name = config.name_transform(name, file)
        end
        result.name = name
        result.file = file
        if checker_name then
          result.checker = checker_name
        end

        local status = result.status
        if results[status] then
          table.insert(results[status], result)
        end
      end
    end
  end

  return results
end

local function report(config)
  local results = collect_results(config)

  local all_results = {}
  for status, items in pairs(results) do
    if type(items) == "table" then
      for _, item in ipairs(items) do
        table.insert(all_results, item)
      end
    end
  end

  local status_icons = common.status_icons()
  print(common.format_results(all_results, status_icons))

  local summary
  if config.summary_format then
    summary = config.summary_format(results)
  else
    summary = common.format_summary(config.checker, results)
  end

  if summary and summary ~= "" then
    print(summary)
  end

  local failures = common.format_failures(all_results)
  if failures then
    print(failures)
  end

  return #results.fail > 0 and 1 or 0
end

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

  if name == "check" then
    -- Multi-checker mode: detect checker type from filename
    local checkers = {"ast-grep", "luacheck", "teal"}
    local checker_files = {}
    local checker_results = {}
    local all_results = {}

    -- Group files by checker type
    for _, file in ipairs(files) do
      for _, checker in ipairs(checkers) do
        local pattern = "%." .. checker:gsub("%-", "%%-") .. "%.ok$"
        if file:match(pattern) then
          if not checker_files[checker] then
            checker_files[checker] = {}
          end
          table.insert(checker_files[checker], file)
          break
        end
      end
    end

    -- Process each checker's files
    for _, checker in ipairs(checkers) do
      if checker_files[checker] then
        local checker_config = {
          files = checker_files[checker],
          strip_suffix = "%." .. checker:gsub("%-", "%%-") .. "%.ok$",
          checker = checker,
          name_transform = config.name_transform,
        }

        local results = collect_results(checker_config)
        checker_results[checker] = results

        -- Add to all_results for combined output
        for status, items in pairs(results) do
          if type(items) == "table" then
            for _, item in ipairs(items) do
              table.insert(all_results, item)
            end
          end
        end
      end
    end

    -- Sort all results by name, then checker
    table.sort(all_results, function(a, b)
      if a.name ~= b.name then
        return a.name < b.name
      end
      return (a.checker or "") < (b.checker or "")
    end)

    -- Print results
    local status_icons = common.status_icons()
    print(common.format_results(all_results, status_icons))

    -- Print per-checker summaries
    local total_checks = 0
    local total_passed = 0
    local total_failed = 0
    local total_skipped = 0
    local total_ignored = 0

    for _, checker in ipairs(checkers) do
      local results = checker_results[checker]
      if results then
        local summary = common.format_summary(checker, results)
        if summary ~= "" then
          print(summary)
          total_checks = total_checks + #results.pass + #results.fail + #results.skip + #results.ignore
          total_passed = total_passed + #results.pass
          total_failed = total_failed + #results.fail
          total_skipped = total_skipped + #results.skip
          total_ignored = total_ignored + #results.ignore
        end
      end
    end

    -- Print total summary
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

    -- Print failures
    local failures = common.format_failures(all_results)
    if failures then
      print(failures)
    end

    return total_failed > 0 and 1 or 0
  elseif name == "test" then
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

  return report(config)
end

if cosmo.is_main() then
  os.exit(main(...))
end
