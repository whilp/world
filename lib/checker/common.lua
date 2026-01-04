-- teal ignore: type annotations needed

local function format_output(status, message, stdout, stderr)
  local lines = {}
  if message and message ~= "" then
    table.insert(lines, status .. ": " .. message)
  else
    table.insert(lines, status)
  end
  table.insert(lines, "")
  table.insert(lines, "## stdout")
  table.insert(lines, "")
  table.insert(lines, stdout or "")
  table.insert(lines, "## stderr")
  table.insert(lines, "")
  table.insert(lines, stderr or "")
  return table.concat(lines, "\n")
end

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

local function strip_prefix(filepath)
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

local function status_icons()
  return {
    pass = "✓",
    fail = "✗",
    skip = "→",
    ignore = "○",
  }
end

local function has_extension(file, extensions)
  for ext in pairs(extensions) do
    if file:sub(-#ext) == ext then
      return true
    end
  end
  return false
end

local function check_first_lines(file, patterns)
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

local function format_results(all_results, icons)
  local lines = {}
  for _, result in ipairs(all_results) do
    local status = string.upper(result.status)
    local icon = icons[result.status] or " "
    local padded = string.format("%-6s", status)
    local line = icon .. " " .. padded .. " " .. result.name
    if result.status ~= "pass" then
      if result.message then
        if result.checker then
          line = line .. " (" .. result.checker .. ": " .. result.message .. ")"
        else
          line = line .. " (" .. result.message .. ")"
        end
      elseif result.checker then
        line = line .. " (" .. result.checker .. ")"
      end
    end
    table.insert(lines, line)
  end
  return table.concat(lines, "\n")
end

local function format_summary(checker_name, results)
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

local function format_failures(all_results)
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

  local lines = {"", "FAILURES:"}
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
        table.insert(lines, result.stdout)
      end
      if result.stderr and result.stderr ~= "" then
        table.insert(lines, "")
        if result.stdout and result.stdout ~= "" then
          table.insert(lines, "stderr:")
        end
        table.insert(lines, result.stderr)
      end
    end
  end

  return table.concat(lines, "\n")
end

local function categorize_results(results_list)
  local results = {
    pass = {},
    fail = {},
    skip = {},
    ignore = {},
  }

  for _, result in ipairs(results_list) do
    local status = result.status
    if results[status] then
      table.insert(results[status], result)
    end
  end

  return results
end

return {
  format_output = format_output,
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
