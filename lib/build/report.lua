-- teal ignore: type annotations needed

local cosmo = require("cosmo")
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

return {
  report = report,
  collect_results = collect_results,
}
