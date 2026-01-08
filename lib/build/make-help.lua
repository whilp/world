#!/usr/bin/env cosmic
-- Parse Makefile targets and extract documentation comments

local path = require("cosmo.path")

-- Parse a makefile and extract documented targets
-- Supports: ## description (line before target)
local function parse_makefile(filepath)
  local file = io.open(filepath, "r")
  if not file then
    return {}
  end

  local targets = {}
  local pending_comment = nil

  for line in file:lines() do
    local trimmed = line:match("^%s*(.-)%s*$")

    -- Comment line: ## description
    local comment = trimmed:match("^##%s*(.*)$")
    if comment then
      if pending_comment then
        pending_comment = pending_comment .. " " .. comment
      else
        pending_comment = comment
      end
      goto continue
    end

    -- Target line: target: deps
    local target_match = trimmed:match("^([a-zA-Z_][a-zA-Z0-9_-]*):%s*(.*)$")
    if target_match then
      if pending_comment then
        table.insert(targets, {
          name = target_match,
          description = pending_comment,
        })
      end
      pending_comment = nil
      goto continue
    end

    -- Non-comment, non-target line resets pending comment
    -- But allow common Makefile directives like .PHONY, .PRECIOUS, etc
    if trimmed ~= "" and not trimmed:match("^#") and not trimmed:match("^%.") then
      pending_comment = nil
    end

    ::continue::
  end

  file:close()
  return targets
end

-- Format help output with colors
local function format_help(targets, options)
  options = options or {}
  local width = options.width or 20
  local use_color = options.color ~= false

  -- ANSI colors
  local cyan = use_color and "\27[36m" or ""
  local bold = use_color and "\27[1m" or ""
  local reset = use_color and "\27[0m" or ""
  local dim = use_color and "\27[2m" or ""

  local output = {}

  table.insert(output, bold .. "Usage:" .. reset .. " make [target] [options]")
  table.insert(output, "")
  table.insert(output, bold .. "Targets:" .. reset)

  for _, item in ipairs(targets) do
    local padding = string.rep(" ", math.max(0, width - #item.name))
    table.insert(output, "  " .. cyan .. item.name .. reset .. padding .. dim .. item.description .. reset)
  end

  -- Add options section
  table.insert(output, "")
  table.insert(output, bold .. "Options:" .. reset)
  table.insert(
    output,
    "  "
      .. cyan
      .. "only=PATTERN"
      .. reset
      .. "      "
      .. dim
      .. "Filter targets by pattern (e.g., make test only=skill)"
      .. reset
  )

  return table.concat(output, "\n")
end

-- Main execution
local function main(args)
  local command = args[1] or "help"
  local makefile = args[2] or "Makefile"

  if command == "help" or command == "--help" then
    local targets = parse_makefile(makefile)
    print(format_help(targets))
    return 0
  elseif command == "list" then
    -- List targets only (for completion, etc)
    local targets = parse_makefile(makefile)
    for _, item in ipairs(targets) do
      print(item.name)
    end
    return 0
  else
    io.stderr:write("Unknown command: " .. command .. "\n")
    io.stderr:write("Usage: help.lua [help|list] [Makefile]\n")
    return 1
  end
end

if arg then
  os.exit(main(arg))
end
