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
  local t0 = hs.timer.secondsSinceEpoch()
  print(string.format("[dispatch] showSwitcher called at %.3f", t0))

  isEmojiMode = false
  isSymbolMode = false

  local t1 = hs.timer.secondsSinceEpoch()
  print(string.format("[dispatch] calling getAllChoices at %.3f", t1))
  local choices = dispatch.getAllChoices(applyFilter)
  local t2 = hs.timer.secondsSinceEpoch()
  print(string.format("[dispatch] getAllChoices returned %d items at %.3f (delta: %.3fms)", #choices, t2, (t2-t1)*1000))

  allChoices = choices
  local debounceTimer = nil

  if not chooser then
    chooser = hs.chooser.new(function(choice)
      local t0 = hs.timer.secondsSinceEpoch()
      print(string.format("[dispatch] callback entered at %.3f", t0))

      if not choice then
        print("[dispatch] choice is nil, returning")
        return
      end

      local t1 = hs.timer.secondsSinceEpoch()
      print(string.format("[dispatch] after nil check at %.3f (delta: %.3fms)", t1, (t1-t0)*1000))

      -- For emoji/symbol modes that need to keep chooser open, handle inline
      if isEmojiMode and choice.emoji then
        print("[dispatch] emoji mode, inserting emoji")
        emojiPicker.insertEmoji(choice.emoji)
        return
      elseif isSymbolMode and choice.symbol then
        print("[dispatch] symbol mode, inserting symbol")
        symbolPicker.insertSymbol(choice.symbol)
        return
      end

      local t2 = hs.timer.secondsSinceEpoch()
      print(string.format("[dispatch] after mode checks at %.3f (delta: %.3fms)", t2, (t2-t1)*1000))

      local choiceType = choice.window and "window" or choice.appName and "app" or choice.commandId and "command" or "unknown"
      print(string.format("[dispatch] choice type: %s", choiceType))

      -- For everything else, close chooser immediately and execute asynchronously
      local t3 = hs.timer.secondsSinceEpoch()
      print(string.format("[dispatch] about to hide chooser at %.3f (delta: %.3fms)", t3, (t3-t2)*1000))

      chooser:hide()

      local t4 = hs.timer.secondsSinceEpoch()
      print(string.format("[dispatch] chooser hidden at %.3f (delta: %.3fms)", t4, (t4-t3)*1000))

      hs.timer.doAfter(0, function()
        local t5 = hs.timer.secondsSinceEpoch()
        print(string.format("[dispatch] timer fired at %.3f (delta: %.3fms from hide)", t5, (t5-t4)*1000))

        if choice.window then
          print("[dispatch] focusing window")
          choice.window:focus()
        elseif choice.appName then
          print(string.format("[dispatch] activating app: %s", choice.appName))
          local app = hs.application.get(choice.appName)
          if app then
            app:activate()
          else
            hs.application.launchOrFocus(choice.appName)
          end
        elseif choice.commandId then
          print(string.format("[dispatch] executing command: %s", choice.commandId))
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

        local t6 = hs.timer.secondsSinceEpoch()
        print(string.format("[dispatch] action completed at %.3f (delta: %.3fms)", t6, (t6-t5)*1000))
        print(string.format("[dispatch] TOTAL TIME: %.3fms", (t6-t0)*1000))
      end)
    end)

    chooserStyle.apply(chooser)

    chooser:queryChangedCallback(function(query)
      local t0 = hs.timer.secondsSinceEpoch()
      print(string.format("[dispatch] queryChanged: '%s' at %.3f", query, t0))

      -- Ignore initial query("") call to avoid double-rendering
      if ignoreNextQueryChange then
        print("[dispatch] ignoring query change (initial open)")
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
        local t1 = hs.timer.secondsSinceEpoch()
        print(string.format("[dispatch] setting %d choices at %.3f (delta: %.3fms)", #limitedChoices, t1, (t1-t0)*1000))
        chooser:choices(limitedChoices)
        local t2 = hs.timer.secondsSinceEpoch()
        print(string.format("[dispatch] choices set at %.3f (delta: %.3fms)", t2, (t2-t1)*1000))
      else
        debounceTimer = hs.timer.doAfter(DEBOUNCE_DELAY, function()
          local t1 = hs.timer.secondsSinceEpoch()
          print(string.format("[dispatch] filter started at %.3f", t1))
          local filtered = filterAndSort(allChoices, query)
          local t2 = hs.timer.secondsSinceEpoch()
          print(string.format("[dispatch] filter completed, setting %d choices at %.3f (delta: %.3fms)", #filtered, t2, (t2-t1)*1000))
          chooser:choices(filtered)
          local t3 = hs.timer.secondsSinceEpoch()
          print(string.format("[dispatch] choices set at %.3f (delta: %.3fms)", t3, (t3-t2)*1000))
        end)
      end
    end)
  end

  -- Start with only top 15 items to avoid initial rendering lag with 129 items
  local initialChoices = {}
  for i = 1, math.min(15, #choices) do
    table.insert(initialChoices, choices[i])
  end

  local t3 = hs.timer.secondsSinceEpoch()
  print(string.format("[dispatch] setting initial choices (%d items) at %.3f", #initialChoices, t3))
  chooser:choices(initialChoices)
  local t4 = hs.timer.secondsSinceEpoch()
  print(string.format("[dispatch] choices set, showing chooser at %.3f (delta: %.3fms)", t4, (t4-t3)*1000))
  ignoreNextQueryChange = true  -- Prevent double-rendering from query("")
  chooser:query("")
  chooser:show()
  chooser:selectedRow(INITIAL_SELECTION)
  local t5 = hs.timer.secondsSinceEpoch()
  print(string.format("[dispatch] chooser shown at %.3f (delta: %.3fms)", t5, (t5-t4)*1000))
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
