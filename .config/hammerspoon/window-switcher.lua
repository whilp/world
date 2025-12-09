local WindowSwitcher = {}

local fuzzy = require("fuzzy")
local dispatch = require("dispatch")
local leaderModal = require("leader-modal")
local emojiPicker = require("emoji-picker")
local symbolPicker = require("symbol-picker")
local chooserStyle = require("chooser-style")
local chooser = nil
local allChoices = {}
local isEmojiMode = false
local isSymbolMode = false
local ignoreNextQueryChange = false

local INITIAL_SELECTION = 2
local SUBTEXT_PENALTY = 50

-- Performance tuning constants
local MAX_RESULTS = 15         -- Limit fuzzy matching to top N results (chooser shows 15 rows)
local DEBOUNCE_DELAY = 0.15    -- Wait 150ms after last keystroke before filtering

-- Enable/disable debug logging
local DEBUG = false

local function debugLog(msg)
  if DEBUG then
    pcall(function() print(msg) end)
  end
end

-- Safe window focus with timeout protection
local function safeFocusWindow(win)
  -- Validate window still exists
  if not win then
    return false
  end

  -- Check if window is still valid by trying to get its app
  local ok, app = pcall(function() return win:application() end)
  if not ok or not app then
    return false
  end

  -- Use AXUIElement directly with timeout for better responsiveness
  local axApp = hs.axuielement.applicationElement(app)
  local axWin = hs.axuielement.windowElement(win)

  if axWin then
    -- Set a timeout on the AX element (500ms)
    axWin:setTimeout(0.5)

    -- Try to raise and focus via AX
    local ok = pcall(function()
      axWin:setAttributeValue("AXMain", true)
      axWin:setAttributeValue("AXFocused", true)
      axApp:setAttributeValue("AXFrontmost", true)
    end)

    if ok then
      return true
    end
  end

  -- Fallback: activate app then focus window
  pcall(function() app:activate(true) end)
  pcall(function() win:focus() end)

  return true
end

local function filterAndSort(choices, query)
  local items = {}
  for _, choice in ipairs(choices) do
    table.insert(items, {
      text = choice.text,
      subText = choice.subText,
      type = dispatch.detectType(choice),
      original = choice
    })
  end

  local results = fuzzy.fuzzy_find(items, query, MAX_RESULTS, SUBTEXT_PENALTY)

  local sortedChoices = {}
  for _, result in ipairs(results) do
    table.insert(sortedChoices, result.original)
  end
  return sortedChoices
end

local function switchToEmojiMode()
  isEmojiMode = true
  isSymbolMode = false
  local emojiChoices = emojiPicker.getEmojiChoices()
  allChoices = emojiChoices
  chooser:choices(emojiChoices)
  chooser:query("")
  chooser:selectedRow(1)
  chooser:show()
end

local function switchToSymbolMode()
  isEmojiMode = false
  isSymbolMode = true
  local symbolChoices = symbolPicker.getSymbolChoices()
  allChoices = symbolChoices
  chooser:choices(symbolChoices)
  chooser:query("")
  chooser:selectedRow(1)
  chooser:show()
end

local function showSwitcher(applyFilter)
  isEmojiMode = false
  isSymbolMode = false
  local choices = dispatch.getAllChoices(applyFilter)
  allChoices = choices
  local debounceTimer = nil

  if not chooser then
    chooser = hs.chooser.new(function(choice)
      if not choice then
        return
      end

      -- Check type field first to avoid slow field probing on window objects
      local choiceType = choice.type

      -- For emoji/symbol modes that need to keep chooser open, handle inline
      if choiceType == "emoji" then
        emojiPicker.insertEmoji(choice.emoji)
        return
      elseif choiceType == "symbol" then
        symbolPicker.insertSymbol(choice.symbol)
        return
      end

      -- For everything else, close chooser immediately and execute asynchronously
      chooser:hide()

      hs.timer.doAfter(0, function()
        if choiceType == "window" then
          safeFocusWindow(choice.window)
        elseif choiceType == "app" then
          local app = hs.application.get(choice.appName)
          if app then
            app:activate()
          else
            hs.application.launchOrFocus(choice.appName)
          end
        elseif choiceType == "command" then
          local action = dispatch.getCommandAction(choice.commandId)
          if action then
            local result = leaderModal.execute_action(action)
            if result == "emoji" then
              switchToEmojiMode()
            elseif result == "symbol" then
              switchToSymbolMode()
            end
          end
        end
      end)
    end)

    chooserStyle.apply(chooser)

    chooser:queryChangedCallback(function(query)
      -- Ignore initial query("") call to avoid double-rendering
      if ignoreNextQueryChange then
        ignoreNextQueryChange = false
        return
      end

      if debounceTimer then
        debounceTimer:stop()
      end

      if query == "" then
        -- Limit to 15 items even when query is empty to avoid rendering lag
        local limitedChoices = {}
        for i = 1, math.min(15, #allChoices) do
          table.insert(limitedChoices, allChoices[i])
        end
        chooser:choices(limitedChoices)
      else
        debounceTimer = hs.timer.doAfter(DEBOUNCE_DELAY, function()
          local filtered = filterAndSort(allChoices, query)
          chooser:choices(filtered)
        end)
      end
    end)
  end

  -- Start with only top 15 items to avoid initial rendering lag
  local initialChoices = {}
  for i = 1, math.min(15, #choices) do
    table.insert(initialChoices, choices[i])
  end

  chooser:choices(initialChoices)
  ignoreNextQueryChange = true  -- Prevent double-rendering from query("")
  chooser:query("")
  chooser:show()
  chooser:selectedRow(INITIAL_SELECTION)
end

function WindowSwitcher.setup(hyper)
  hyper:bind("tab"):toFunction("Window switcher", function()
    showSwitcher(true)
  end)
  hyper:bind("shift", "tab"):toFunction("Window switcher (unfiltered)", function()
    showSwitcher(false)
  end)
end

function WindowSwitcher.showUnfiltered()
  showSwitcher(false)
end

return WindowSwitcher
