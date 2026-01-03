-- teal ignore: type annotations needed

local M = {}

function M.format_output(status, message, stdout, stderr)
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

function M.parse_result(content)
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

function M.strip_prefix(filepath)
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

function M.status_icons()
  return {
    pass = "✓",
    fail = "✗",
    skip = "→",
    ignore = "○",
  }
end

function M.has_extension(file, extensions)
  for ext in pairs(extensions) do
    if file:sub(-#ext) == ext then
      return true
    end
  end
  return false
end

function M.check_first_lines(file, patterns)
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

function M.print_results(all_results, icons)
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
    print(line)
  end
end

function M.print_summary(checker_name, results)
  local total = #results.pass + #results.fail + #results.skip + #results.ignore
  if total == 0 then
    return
  end

  print(string.format(
    "%s: %d checks: %d passed, %d failed, %d skipped, %d ignored",
    checker_name,
    total,
    #results.pass,
    #results.fail,
    #results.skip,
    #results.ignore
  ))
end

function M.print_failures(all_results)
  local has_failures = false
  for _, result in ipairs(all_results) do
    if result.status == "fail" then
      has_failures = true
      break
    end
  end

  if not has_failures then
    return false
  end

  print("")
  print("FAILURES:")
  for _, result in ipairs(all_results) do
    if result.status == "fail" then
      print("")
      if result.checker then
        print(string.format("--- %s (%s) ---", result.name, result.checker))
      else
        print(string.format("--- %s ---", result.name))
      end
      if result.message then
        print(result.message)
      end
      if result.stderr and result.stderr ~= "" then
        print("")
        print(result.stderr)
      end
    end
  end

  return true
end

function M.categorize_results(results_list)
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

return M
