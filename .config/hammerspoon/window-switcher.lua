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

local INITIAL_SELECTION = 2
local SUBTEXT_PENALTY = 50

-- Performance tuning constants
local MAX_RESULTS = 100        -- Limit fuzzy matching to top N results (chooser shows 15 rows)
local DEBOUNCE_DELAY = 0.05    -- Wait 50ms after last keystroke before filtering

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
      if choice then
        if isEmojiMode and choice.emoji then
          emojiPicker.insertEmoji(choice.emoji)
        elseif isSymbolMode and choice.symbol then
          symbolPicker.insertSymbol(choice.symbol)
        elseif choice.window then
          choice.window:focus()
        elseif choice.appName then
          local app = hs.application.get(choice.appName)
          if app then
            app:activate()
          else
            hs.application.launchOrFocus(choice.appName)
          end
        elseif choice.commandId then
          local action = dispatch.getCommandAction(choice.commandId)
          if action then
            local result = leaderModal.execute_action(action)
            if result == "emoji" then
              switchToEmojiMode()
              return
            elseif result == "symbol" then
              switchToSymbolMode()
              return
            end
          end
        end
      end
    end)

    chooserStyle.apply(chooser)

    chooser:queryChangedCallback(function(query)
      if debounceTimer then
        debounceTimer:stop()
      end

      if query == "" then
        chooser:choices(allChoices)
      else
        debounceTimer = hs.timer.doAfter(DEBOUNCE_DELAY, function()
          chooser:choices(filterAndSort(allChoices, query))
        end)
      end
    end)
  end

  chooser:choices(choices)
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
