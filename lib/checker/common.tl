local MAX_CONSOLE_LINES = 10

local record CheckResult
  status: string
  message: string
  name: string
  checker: string
  stdout: string
  stderr: string
end

local record CheckPatterns
  shebangs: {string: boolean}
  ignore: string
end

local record CategorizedResults
  pass: {CheckResult}
  fail: {CheckResult}
  skip: {CheckResult}
  ignore: {CheckResult}
end

local record StatusIcons
  pass: string
  fail: string
  skip: string
  ignore: string
end

local function truncate_lines(text: string, max_lines: integer): string
  if not text or text == "" then
    return text
  end
  local lines: {string} = {}
  local count = 0
  for line in text:gmatch("[^\n]*") do
    count = count + 1
    if count <= max_lines then
      table.insert(lines, line)
    end
  end
  if count > max_lines then
    table.insert(lines, string.format("... (%d more lines)", count - max_lines))
  end
  return table.concat(lines, "\n")
end

local function format_output(status: string, message: string, stdout: string, stderr: string, truncate: boolean): string
  local lines: {string} = {}
  if message and message ~= "" then
    table.insert(lines, status .. ": " .. message)
  else
    table.insert(lines, status)
  end
  table.insert(lines, "")
  table.insert(lines, "## stdout")
  table.insert(lines, "")
  table.insert(lines, truncate and truncate_lines(stdout, MAX_CONSOLE_LINES) or (stdout or ""))
  table.insert(lines, "## stderr")
  table.insert(lines, "")
  table.insert(lines, truncate and truncate_lines(stderr, MAX_CONSOLE_LINES) or (stderr or ""))
  return table.concat(lines, "\n")
end

local function write_result(status: string, message: string, stdout: string, stderr: string, _source: string): integer
  -- write full output to stdout for capture
  local output = format_output(status, message, stdout, stderr, false)
  io.write(output)
  return 0
end

local function parse_result(content: string): CheckResult
  local result: CheckResult = {}

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

local function strip_prefix(filepath: string): string
  local prefix = os.getenv("TEST_O")
  if not prefix then
    return filepath
  end
  local prefix_len = #prefix
  if filepath:sub(1, prefix_len) == prefix and filepath:sub(prefix_len + 1, prefix_len + 1) == "/" then
    return filepath:sub(prefix_len + 2)
  end
  return filepath
end

local function status_icons(): StatusIcons
  return {
    pass = "✓",
    fail = "✗",
    skip = "→",
    ignore = "○",
  }
end

local function has_extension(file: string, extensions: {string: boolean}): boolean
  for ext in pairs(extensions) do
    if file:sub(-#ext) == ext then
      return true
    end
  end
  return false
end

local function check_first_lines(file: string, patterns: CheckPatterns): boolean, string
  local f = io.open(file, "r")
  if not f then
    return nil, nil
  end

  local supported_shebangs = patterns.shebangs or {}
  local ignore_pattern = patterns.ignore

  local has_shebang = false
  for i = 1, 10 do
    local line = f:read("*l")
    if not line then
      break
    end

    if i == 1 then
      local interp = line:match("^#!.-/([%w_-]+)%s*$") or line:match("^#!/usr/bin/env%s+([%w_-]+)")
      if interp and supported_shebangs[interp] then
        has_shebang = true
      end
    end

    if ignore_pattern then
      local reason = line:match(ignore_pattern)
      if reason then
        f:close()
        return has_shebang, reason ~= "" and reason or "directive"
      end
    end
  end

  f:close()
  return has_shebang, nil
end

local CHECKER_WIDTH = 8  -- width of longest checker name (ast-grep)

local function format_results(all_results: {CheckResult}, icons: StatusIcons): string
  local lines: {string} = {}
  for _, result in ipairs(all_results) do
    local status = string.upper(result.status)
    local icon = (icons as {string: string})[result.status] or " "
    local checker = result.checker or ""
    local line = string.format("%s %-6s %-" .. CHECKER_WIDTH .. "s %s",
      icon, status, checker, result.name)
    if result.status ~= "pass" and result.message then
      line = line .. " (" .. result.message .. ")"
    end
    table.insert(lines, line)
  end
  return table.concat(lines, "\n")
end

local function format_summary(checker_name: string, results: CategorizedResults): string
  local total = #results.pass + #results.fail + #results.skip + #results.ignore
  if total == 0 then
    return ""
  end

  return string.format(
    "%s: %d checks: %d passed, %d failed, %d skipped, %d ignored",
    checker_name,
    total,
    #results.pass,
    #results.fail,
    #results.skip,
    #results.ignore
  )
end

local function format_failures(all_results: {CheckResult}): string
  local has_failures = false
  for _, result in ipairs(all_results) do
    if result.status == "fail" then
      has_failures = true
      break
    end
  end

  if not has_failures then
    return nil
  end

  local lines: {string} = {"", "FAILURES:"}
  for _, result in ipairs(all_results) do
    if result.status == "fail" then
      table.insert(lines, "")
      if result.checker then
        table.insert(lines, string.format("--- %s (%s) ---", result.name, result.checker))
      else
        table.insert(lines, string.format("--- %s ---", result.name))
      end
      if result.message then
        table.insert(lines, result.message)
      end
      if result.stdout and result.stdout ~= "" then
        table.insert(lines, "")
        table.insert(lines, "stdout:")
        table.insert(lines, truncate_lines(result.stdout, MAX_CONSOLE_LINES))
      end
      if result.stderr and result.stderr ~= "" then
        table.insert(lines, "")
        if result.stdout and result.stdout ~= "" then
          table.insert(lines, "stderr:")
        end
        table.insert(lines, truncate_lines(result.stderr, MAX_CONSOLE_LINES))
      end
    end
  end

  return table.concat(lines, "\n")
end

local function categorize_results(results_list: {CheckResult}): CategorizedResults
  local results: CategorizedResults = {
    pass = {},
    fail = {},
    skip = {},
    ignore = {},
  }

  for _, result in ipairs(results_list) do
    local status = result.status
    if (results as {string: {CheckResult}})[status] then
      table.insert((results as {string: {CheckResult}})[status], result)
    end
  end

  return results
end

return {
  format_output = format_output,
  write_result = write_result,
  parse_result = parse_result,
  strip_prefix = strip_prefix,
  status_icons = status_icons,
  has_extension = has_extension,
  check_first_lines = check_first_lines,
  format_results = format_results,
  format_summary = format_summary,
  format_failures = format_failures,
  categorize_results = categorize_results,
}
