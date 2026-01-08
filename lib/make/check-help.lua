#!/usr/bin/env cosmic
-- Validate that Makefile help documentation is accurate

local path = require("cosmo.path")

-- Parse documented targets from Makefile (same logic as help.lua)
local function parse_documented_targets(filepath)
  local file = io.open(filepath, "r")
  if not file then
    return {}
  end

  local targets = {}
  local pending_comment = nil

  for line in file:lines() do
    local trimmed = line:match("^%s*(.-)%s*$")

    -- Skip section headers
    local section = trimmed:match("^##%s*@%s*(.+)$")
    if section then
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

      -- Check for inline comment
      local inline_desc = rest:match("##%s*(.+)$")
      if inline_desc or pending_comment then
        targets[target_name] = true
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

-- Parse .PHONY targets from Makefile
local function parse_phony_targets(filepath)
  local file = io.open(filepath, "r")
  if not file then
    return {}
  end

  local phony_targets = {}

  for line in file:lines() do
    local trimmed = line:match("^%s*(.-)%s*$")

    -- Match .PHONY: target1 target2 ...
    local phony_line = trimmed:match("^%.PHONY:%s*(.+)$")
    if phony_line then
      -- Split on whitespace
      for target in phony_line:gmatch("%S+") do
        phony_targets[target] = true
      end
    end
  end

  file:close()
  return phony_targets
end

-- Main validation
local function main(args)
  local makefile = args[1] or "Makefile"

  local documented = parse_documented_targets(makefile)
  local phony = parse_phony_targets(makefile)

  local errors = {}
  local warnings = {}

  -- Check for documented targets that aren't .PHONY
  for target, _ in pairs(documented) do
    if not phony[target] then
      table.insert(warnings, string.format("'%s' is documented but not marked .PHONY", target))
    end
  end

  -- Check for .PHONY targets that aren't documented
  -- Exclude some internal/generated targets
  local exclude_patterns = {
    "^3p-",    -- platform-specific 3p targets
    "^debug-", -- debug targets
  }

  for target, _ in pairs(phony) do
    local should_exclude = false
    for _, pattern in ipairs(exclude_patterns) do
      if target:match(pattern) then
        should_exclude = true
        break
      end
    end

    if not should_exclude and not documented[target] then
      table.insert(
        warnings,
        string.format("'%s' is .PHONY but not documented (consider adding ## comment)", target)
      )
    end
  end

  -- Sort for consistent output
  table.sort(errors)
  table.sort(warnings)

  local exit_code = 0

  if #errors > 0 then
    io.stderr:write("\nErrors:\n")
    for _, err in ipairs(errors) do
      io.stderr:write("  ✗ " .. err .. "\n")
    end
    exit_code = 1
  end

  if #warnings > 0 then
    print("\nWarnings:")
    for _, warn in ipairs(warnings) do
      print("  ⚠ " .. warn)
    end
  end

  if #errors == 0 and #warnings == 0 then
    print("✓ All help documentation is accurate")
  end

  print(
    string.format(
      "\nSummary: %d documented targets, %d .PHONY targets",
      #vim.tbl_keys(documented or {}),
      #vim.tbl_keys(phony or {})
    )
  )

  return exit_code
end

-- Helper to count table keys (since we don't have vim.tbl_keys)
local function count_keys(t)
  local count = 0
  for _, _ in pairs(t) do
    count = count + 1
  end
  return count
end

if arg then
  -- Fix the summary line to not use vim.tbl_keys
  local function main_wrapper(args)
    local makefile = args[1] or "Makefile"

    local documented = parse_documented_targets(makefile)
    local phony = parse_phony_targets(makefile)

    local errors = {}
    local warnings = {}

    -- Note: We don't warn about documented targets that aren't .PHONY
    -- because some targets intentionally produce files for incremental builds
    -- (e.g., test -> $(o)/test-summary.txt)

    -- Check for .PHONY targets that aren't documented
    local exclude_patterns = {
      "^3p-",
      "^debug-",
    }

    for target, _ in pairs(phony) do
      local should_exclude = false
      for _, pattern in ipairs(exclude_patterns) do
        if target:match(pattern) then
          should_exclude = true
          break
        end
      end

      if not should_exclude and not documented[target] then
        table.insert(
          warnings,
          string.format("'%s' is .PHONY but not documented (consider adding ## comment)", target)
        )
      end
    end

    table.sort(errors)
    table.sort(warnings)

    local exit_code = 0

    if #errors > 0 then
      io.stderr:write("\nErrors:\n")
      for _, err in ipairs(errors) do
        io.stderr:write("  ✗ " .. err .. "\n")
      end
      exit_code = 1
    end

    if #warnings > 0 then
      print("\nWarnings:")
      for _, warn in ipairs(warnings) do
        print("  ⚠ " .. warn)
      end
    end

    if #errors == 0 and #warnings == 0 then
      print("✓ All help documentation is accurate")
    end

    print(
      string.format(
        "\nSummary: %d documented targets, %d .PHONY targets",
        count_keys(documented),
        count_keys(phony)
      )
    )

    return exit_code
  end

  os.exit(main_wrapper(arg))
end
