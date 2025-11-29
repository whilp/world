local function run_tests()
  print("\n=== Clue Loader Test Suite ===\n")

  package.loaded["clue-loader"] = nil

  local success, loader = pcall(require, "clue-loader")
  if not success then
    print("ERROR: Failed to load clue-loader module")
    print("Error: " .. tostring(loader))
    return false
  end

  print("✓ Successfully loaded clue-loader module\n")

  print("--- Phase 1: Loading clue files ---")
  local ok, err = pcall(function()
    loader.load_all()
  end)

  if not ok then
    print("ERROR: Failed to load clues")
    print("Error: " .. tostring(err))
    return false
  end

  print("✓ Successfully loaded all clue files\n")

  print("--- Phase 2: Analyzing loaded clues ---")
  local clue_count = 0
  local clue_details = {}

  for id, clue in pairs(loader.clues) do
    clue_count = clue_count + 1
    table.insert(clue_details, {
      id = id,
      name = clue.name,
      desc = clue.desc or "no description",
      group = clue.group or "no group",
      key = table.concat(clue.key, " + "),
    })
  end

  table.sort(clue_details, function(a, b) return a.id < b.id end)

  print(string.format("Total clues loaded: %d\n", clue_count))

  if clue_count > 0 then
    print("Clue details:")
    for _, detail in ipairs(clue_details) do
      print(string.format("  [%s]", detail.id))
      print(string.format("    Name:  %s", detail.name))
      print(string.format("    Desc:  %s", detail.desc))
      print(string.format("    Group: %s", detail.group))
      print(string.format("    Keys:  %s", detail.key))
      print()
    end
  else
    print("WARNING: No clues were loaded!\n")
  end

  print("--- Phase 3: Analyzing modals ---")
  local modal_count = 0
  local modal_details = {}

  for prefix, modal in pairs(loader.modals) do
    modal_count = modal_count + 1
    local binding_count = #modal.bindings
    table.insert(modal_details, {
      prefix = prefix,
      trigger = table.concat(modal.trigger, " + "),
      binding_count = binding_count,
      bindings = modal.bindings,
    })
  end

  table.sort(modal_details, function(a, b) return a.prefix < b.prefix end)

  print(string.format("Total modals generated: %d\n", modal_count))

  if modal_count > 0 then
    print("Modal details:")
    for _, detail in ipairs(modal_details) do
      print(string.format("  Prefix: %s", detail.prefix))
      print(string.format("  Trigger: %s", detail.trigger))
      print(string.format("  Bindings: %d", detail.binding_count))

      for _, binding in ipairs(detail.bindings) do
        print(string.format("    %s → %s", binding.key, binding.clue.name))
      end
      print()
    end
  else
    print("WARNING: No modals were generated!\n")
  end

  print("--- Phase 4: Validating test.lua clues ---")
  local expected_test_clues = {
    "test-alert",
    "test-notify",
    "test-console"
  }

  local found_test_clues = {}
  local missing_test_clues = {}

  for _, expected_id in ipairs(expected_test_clues) do
    local clue = loader.get_clue(expected_id)
    if clue then
      table.insert(found_test_clues, expected_id)
      print(string.format("✓ Found expected test clue: %s (%s)", expected_id, clue.name))
    else
      table.insert(missing_test_clues, expected_id)
      print(string.format("✗ Missing expected test clue: %s", expected_id))
    end
  end

  print()

  print("--- Phase 5: Testing group functionality ---")
  local test_group = loader.get_clues_for_group("test")
  print(string.format("Clues in 'test' group: %d", #test_group))

  if #test_group > 0 then
    for _, clue in ipairs(test_group) do
      print(string.format("  - %s (%s)", clue.name, clue.id))
    end
    print()
  end

  print("--- Phase 6: Testing chooser integration ---")
  local choices = loader.to_choices()
  print(string.format("Chooser choices generated: %d", #choices))

  if #choices > 0 then
    print("Sample choices:")
    for i = 1, math.min(3, #choices) do
      local choice = choices[i]
      print(string.format("  %d. %s", i, choice.text))
      print(string.format("     SubText: %s", choice.subText))
      print(string.format("     CommandID: %s", choice.commandId))
    end
    print()
  end

  print("--- Summary ---")
  print(string.format("Clues loaded: %d", clue_count))
  print(string.format("Modals generated: %d", modal_count))
  print(string.format("Test clues found: %d/%d", #found_test_clues, #expected_test_clues))
  print(string.format("Test group size: %d", #test_group))
  print(string.format("Chooser choices: %d", #choices))

  local all_passed = clue_count > 0
    and modal_count > 0
    and #found_test_clues == #expected_test_clues
    and #test_group == #expected_test_clues

  print()
  if all_passed then
    print("✓✓✓ All tests PASSED ✓✓✓")
  else
    print("✗✗✗ Some tests FAILED ✗✗✗")
  end
  print()

  return all_passed
end

local success, result = pcall(run_tests)
if not success then
  print("\n=== TEST SUITE CRASHED ===")
  print("Error: " .. tostring(result))
  print()
end
