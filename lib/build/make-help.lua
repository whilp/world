#!/usr/bin/env cosmic
-- teal ignore: type annotations needed
-- Parse Makefile targets and extract documentation comments

local cosmo = require("cosmo")
local path = require("cosmo.path")

-- Parse a makefile and extract documented targets and options
-- Supports: ## description (line before target or variable)
local function parse_makefile(content)
  local items = {}
  local pending_comment = nil

  for line in content:gmatch("[^\r\n]+") do
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
        table.insert(items, {
          type = "target",
          name = target_match,
          description = pending_comment,
        })
      end
      pending_comment = nil
      goto continue
    end

    -- Variable assignment: name = value or name := value or name ?= value
    local var_match = trimmed:match("^([a-zA-Z_][a-zA-Z0-9_-]*)%s*[?:]?=%s*")
    if var_match then
      if pending_comment then
        table.insert(items, {
          type = "option",
          name = var_match,
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

  return items
end

-- Format help output
local function format_help(items, options)
  options = options or {}
  local width = options.width or 20

  local output = {}
  local targets = {}
  local vars = {}

  -- Separate targets and options
  for _, item in ipairs(items) do
    if item.type == "target" then
      table.insert(targets, item)
    elseif item.type == "option" then
      table.insert(vars, item)
    end
  end

  table.insert(output, "Usage: make [target] [options]")
  table.insert(output, "")
  table.insert(output, "Targets:")

  for _, item in ipairs(targets) do
    local padding = string.rep(" ", math.max(0, width - #item.name))
    table.insert(output, "  " .. item.name .. padding .. item.description)
  end

  -- Add options section if we have any
  if #vars > 0 then
    table.insert(output, "")
    table.insert(output, "Options:")
    for _, item in ipairs(vars) do
      local padding = string.rep(" ", math.max(0, width - #item.name))
      table.insert(output, "  " .. item.name .. padding .. item.description)
    end
  end

  return table.concat(output, "\n")
end

-- Main execution
local function main(args)
  local makefile = args[1] or "Makefile"

  local content, err = cosmo.Slurp(makefile)
  if not content then
    return 1, err or ("Failed to read " .. makefile)
  end

  local targets = parse_makefile(content)
  print(format_help(targets))
  return 0
end

if cosmo.is_main() then
  local exit_code, err = main(arg)
  if err then
    io.stderr:write(err .. "\n")
  end
  os.exit(exit_code)
end

return {
  parse_makefile = parse_makefile,
  format_help = format_help,
}
