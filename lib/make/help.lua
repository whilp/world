#!/usr/bin/env cosmic
-- Parse Makefile targets and extract documentation comments

local path = require("cosmo.path")

-- Parse a makefile and extract documented targets
-- Supports:
-- 1. Inline: target: deps ## description
-- 2. Block: ## description (line before target)
-- 3. Headers: ## @ Section Name (no target following)
local function parse_makefile(filepath)
  local file = io.open(filepath, "r")
  if not file then
    return {}
  end

  local targets = {}
  local pending_comment = nil
  local current_section = nil

  for line in file:lines() do
    -- Strip leading/trailing whitespace
    local trimmed = line:match("^%s*(.-)%s*$")

    -- Section header: ## @ Section Name
    local section = trimmed:match("^##%s*@%s*(.+)$")
    if section then
      current_section = section
      table.insert(targets, { type = "section", name = section })
      pending_comment = nil
      goto continue
    end

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
      local target_name = target_match
      local rest = trimmed:match("^[^:]+:%s*(.*)$")

      -- Check for inline comment: ## description
      local inline_desc = rest:match("##%s*(.+)$")
      local description = inline_desc or pending_comment

      if description then
        table.insert(targets, {
          type = "target",
          name = target_name,
          description = description,
          section = current_section,
        })
      end

      pending_comment = nil
      goto continue
    end

    -- Non-comment, non-target line resets pending comment
    if trimmed ~= "" and not trimmed:match("^#") then
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

  -- Header
  table.insert(output, bold .. "Usage:" .. reset .. " make [target] [options]")
  table.insert(output, "")

  local current_section = nil
  for _, item in ipairs(targets) do
    if item.type == "section" then
      current_section = item.name
      table.insert(output, "")
      table.insert(output, bold .. item.name .. ":" .. reset)
    elseif item.type == "target" then
      -- If no section yet, use "Targets:"
      if not current_section and #output == 2 then
        table.insert(output, bold .. "Targets:" .. reset)
      end

      local padding = string.rep(" ", math.max(0, width - #item.name))
      table.insert(
        output,
        "  " .. cyan .. item.name .. reset .. padding .. dim .. item.description .. reset
      )
    end
  end

  -- Add options section
  table.insert(output, "")
  table.insert(output, bold .. "Options:" .. reset)
  table.insert(output, "  " .. cyan .. "only=PATTERN" .. reset .. "      " .. dim .. "Filter targets by pattern (e.g., make test only=skill)" .. reset)

  return table.concat(output, "\n")
end

-- Validate documented targets exist in make
local function validate_targets(targets)
  local errors = {}

  for _, item in ipairs(targets) do
    if item.type == "target" then
      -- Try to run make -n <target> to see if it exists
      local cmd = string.format("make -n %s >/dev/null 2>&1", item.name)
      local ok = os.execute(cmd)
      if not ok then
        table.insert(errors, string.format("Target '%s' is documented but may not exist", item.name))
      end
    end
  end

  return errors
end

-- Main execution
local function main(args)
  local command = args[1] or "help"
  local makefile = args[2] or "Makefile"

  if command == "help" or command == "--help" then
    local targets = parse_makefile(makefile)
    print(format_help(targets))
    return 0
  elseif command == "validate" then
    local targets = parse_makefile(makefile)
    local errors = validate_targets(targets)

    if #errors > 0 then
      io.stderr:write("Validation errors:\n")
      for _, err in ipairs(errors) do
        io.stderr:write("  • " .. err .. "\n")
      end
      return 1
    else
      print("✓ All documented targets are valid")
      return 0
    end
  elseif command == "list" then
    -- List targets only (for completion, etc)
    local targets = parse_makefile(makefile)
    for _, item in ipairs(targets) do
      if item.type == "target" then
        print(item.name)
      end
    end
    return 0
  else
    io.stderr:write("Unknown command: " .. command .. "\n")
    io.stderr:write("Usage: help.lua [help|validate|list] [Makefile]\n")
    return 1
  end
end

if arg then
  os.exit(main(arg))
end
