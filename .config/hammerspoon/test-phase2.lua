-- Phase 2 implementation test
-- validates clue loading, grouping, and modal generation
-- Run from Hammerspoon console: dofile(hs.configdir .. "/test-phase2.lua")

local function separator(char)
  print(string.rep(char or "=", 80))
end

local function header(text)
  separator("=")
  print("  " .. text)
  separator("=")
  print()
end

local function subheader(text)
  separator("-")
  print("  " .. text)
  separator("-")
end

local function test_phase2()
  local errors = {}
  local warnings = {}
  local passes = 0
  local total_tests = 0

  local function test(name, fn)
    total_tests = total_tests + 1
    local ok, result = pcall(fn)
    if not ok then
      table.insert(errors, {name = name, error = result})
      print("  ✗ " .. name)
      print("    Error: " .. tostring(result))
      return false
    elseif result == false then
      table.insert(warnings, {name = name})
      print("  ⚠ " .. name)
      return false
    else
      passes = passes + 1
      print("  ✓ " .. name)
      return true
    end
  end

  header("Phase 2 implementation test")

  -- load modules
  print("Loading modules...")
  local loader = require("clue-loader")
  local manager = require("clue-manager")
  print("  ✓ clue-loader loaded")
  print("  ✓ clue-manager loaded")
  print()

  -- reset state
  loader.clues = {}
  loader.modals = {}
  loader.groups = {}

  -- load all clue files
  subheader("Loading clue files")
  local ok, err = pcall(function()
    loader.load_all()
  end)

  if not ok then
    print("  ✗ Failed to load clues: " .. tostring(err))
    return
  end
  print("  ✓ All clue files loaded successfully")
  print()

  -- count clues by group
  subheader("Clue counts by group")
  local group_counts = {}
  for group_name, clues in pairs(loader.groups) do
    group_counts[group_name] = #clues
    print(string.format("  %s: %d clues", group_name, #clues))
  end
  print()

  -- validate expected counts
  subheader("Validating clue counts")
  test("cleanshot group has 9 clues", function()
    if group_counts.cleanshot ~= 9 then
      error("expected 9, got " .. tostring(group_counts.cleanshot))
    end
  end)

  test("system group has 13 clues", function()
    if group_counts.system ~= 13 then
      error("expected 13, got " .. tostring(group_counts.system))
    end
  end)

  test("hammerspoon group has 5 clues", function()
    if group_counts.hammerspoon ~= 5 then
      error("expected 5, got " .. tostring(group_counts.hammerspoon))
    end
  end)

  test("test group has 3 clues", function()
    if group_counts.test ~= 3 then
      error("expected 3, got " .. tostring(group_counts.test))
    end
  end)

  -- count total clues
  local total_clues = 0
  for _, clue in pairs(loader.clues) do
    total_clues = total_clues + 1
  end

  test("total clues equals 30", function()
    if total_clues ~= 30 then
      error("expected 30, got " .. tostring(total_clues))
    end
  end)
  print()

  -- validate modal generation
  subheader("Modal generation")
  local modal_count = 0
  for prefix, _ in pairs(loader.modals) do
    modal_count = modal_count + 1
  end

  print(string.format("  Total modals generated: %d", modal_count))
  print()

  test("exactly 4 modals generated", function()
    if modal_count ~= 4 then
      error("expected 4, got " .. tostring(modal_count))
    end
  end)

  test("hyper:c modal exists", function()
    if not loader.modals["hyper:c"] then
      error("modal not found")
    end
  end)

  test("hyper:s modal exists", function()
    if not loader.modals["hyper:s"] then
      error("modal not found")
    end
  end)

  test("hyper:h modal exists", function()
    if not loader.modals["hyper:h"] then
      error("modal not found")
    end
  end)

  test("hyper:t modal exists", function()
    if not loader.modals["hyper:t"] then
      error("modal not found")
    end
  end)
  print()

  -- validate modal structure
  subheader("Modal structure validation")
  for prefix, modal in pairs(loader.modals) do
    print(string.format("  Modal: %s", prefix))

    test(prefix .. " has trigger", function()
      if not modal.trigger then
        error("missing trigger")
      end
    end)

    test(prefix .. " has bindings", function()
      if not modal.bindings then
        error("missing bindings")
      end
    end)

    test(prefix .. " trigger is table", function()
      if type(modal.trigger) ~= "table" then
        error("trigger is not a table")
      end
    end)

    test(prefix .. " trigger has 2 elements", function()
      if #modal.trigger ~= 2 then
        error("expected 2 elements, got " .. tostring(#modal.trigger))
      end
    end)

    print(string.format("    Trigger: %s", table.concat(modal.trigger, " + ")))
    print(string.format("    Bindings: %d", #modal.bindings))
    print()
  end

  -- detailed modal inspection
  subheader("Detailed modal inspection")
  local modal_prefixes = {"hyper:c", "hyper:s", "hyper:h", "hyper:t"}
  local expected_bindings = {
    ["hyper:c"] = 9,
    ["hyper:s"] = 13,
    ["hyper:h"] = 5,
    ["hyper:t"] = 3
  }

  for _, prefix in ipairs(modal_prefixes) do
    local modal = loader.modals[prefix]
    if modal then
      print(string.format("Modal: %s", prefix))
      print(string.format("  Trigger: [%s, %s]", modal.trigger[1], modal.trigger[2]))
      print(string.format("  Bindings: %d", #modal.bindings))

      test(prefix .. " has expected binding count", function()
        local expected = expected_bindings[prefix]
        if #modal.bindings ~= expected then
          error(string.format("expected %d, got %d", expected, #modal.bindings))
        end
      end)

      -- sort bindings by key for display
      local sorted = {}
      for _, binding in ipairs(modal.bindings) do
        table.insert(sorted, binding)
      end
      table.sort(sorted, function(a, b)
        return a.key < b.key
      end)

      print("  Keys:")
      for _, binding in ipairs(sorted) do
        local clue = binding.clue
        print(string.format("    %s → %s", binding.key, clue.name))
      end
      print()
    end
  end

  -- validate clue ids
  subheader("Clue ID validation")
  test("all clues have IDs", function()
    for id, clue in pairs(loader.clues) do
      if not clue.id then
        error("clue missing ID: " .. tostring(clue.name))
      end
    end
  end)

  test("all clue IDs are strings", function()
    for id, clue in pairs(loader.clues) do
      if type(clue.id) ~= "string" then
        error("clue ID is not string: " .. tostring(clue.name))
      end
    end
  end)

  test("clue IDs match expected format", function()
    for id, clue in pairs(loader.clues) do
      if not clue.id:match("^[%w-]+$") then
        error("clue ID has invalid format: " .. clue.id)
      end
    end
  end)

  test("no duplicate clue IDs", function()
    local seen = {}
    for id, clue in pairs(loader.clues) do
      if seen[clue.id] then
        error("duplicate ID: " .. clue.id)
      end
      seen[clue.id] = true
    end
  end)
  print()

  -- validate clue structure
  subheader("Clue structure validation")
  test("all clues have name", function()
    for id, clue in pairs(loader.clues) do
      if not clue.name then
        error("clue missing name: " .. id)
      end
    end
  end)

  test("all clues have key", function()
    for id, clue in pairs(loader.clues) do
      if not clue.key then
        error("clue missing key: " .. id)
      end
    end
  end)

  test("all clues have action", function()
    for id, clue in pairs(loader.clues) do
      if not clue.action then
        error("clue missing action: " .. id)
      end
    end
  end)

  test("all clue keys are tables", function()
    for id, clue in pairs(loader.clues) do
      if type(clue.key) ~= "table" then
        error("clue key is not table: " .. id)
      end
    end
  end)

  test("all clue keys have 3 elements", function()
    for id, clue in pairs(loader.clues) do
      if #clue.key ~= 3 then
        error(string.format("clue key has %d elements: %s", #clue.key, id))
      end
    end
  end)

  test("all clue actions are tables", function()
    for id, clue in pairs(loader.clues) do
      if type(clue.action) ~= "table" then
        error("clue action is not table: " .. id)
      end
    end
  end)

  test("all clue actions have exactly one action type", function()
    for id, clue in pairs(loader.clues) do
      local action_count = 0
      if clue.action.url then action_count = action_count + 1 end
      if clue.action.fn then action_count = action_count + 1 end
      if clue.action.shell then action_count = action_count + 1 end
      if clue.action.focus then action_count = action_count + 1 end
      if clue.action.mode then action_count = action_count + 1 end

      if action_count ~= 1 then
        error(string.format("clue has %d action types: %s", action_count, id))
      end
    end
  end)
  print()

  -- validate group membership
  subheader("Group membership validation")
  test("all clues belong to a group", function()
    for id, clue in pairs(loader.clues) do
      if not clue.group then
        error("clue has no group: " .. id)
      end
    end
  end)

  test("all groups are valid", function()
    local valid_groups = {cleanshot = true, system = true, hammerspoon = true, test = true}
    for id, clue in pairs(loader.clues) do
      if not valid_groups[clue.group] then
        error("invalid group: " .. tostring(clue.group) .. " for clue " .. id)
      end
    end
  end)

  test("group lists match clue group assignments", function()
    for group_name, clues in pairs(loader.groups) do
      for _, clue in ipairs(clues) do
        if clue.group ~= group_name then
          error(string.format("clue %s in group %s but has group %s",
            clue.id, group_name, clue.group))
        end
      end
    end
  end)
  print()

  -- summary
  separator("=")
  print()
  header("Test summary")
  print(string.format("  Total tests: %d", total_tests))
  print(string.format("  Passed: %d", passes))
  print(string.format("  Warnings: %d", #warnings))
  print(string.format("  Errors: %d", #errors))
  print()

  if #errors > 0 then
    subheader("Errors")
    for _, err in ipairs(errors) do
      print(string.format("  %s: %s", err.name, err.error))
    end
    print()
  end

  if #warnings > 0 then
    subheader("Warnings")
    for _, warn in ipairs(warnings) do
      print(string.format("  %s", warn.name))
    end
    print()
  end

  separator("=")
  if #errors == 0 then
    print("✓ Phase 2 implementation validated successfully!")
  else
    print("✗ Phase 2 implementation has errors")
  end
  separator("=")
  print()

  return #errors == 0
end

-- run the test
local ok, result = pcall(test_phase2)
if not ok then
  print("\n✗ Test failed with error:")
  print(result)
  return false
end

return result
