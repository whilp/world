local WindowSwitcher = {}

local fuzzy = require("fuzzy")
local dispatch = require("dispatch")
local hammerspoonModule = require("hammerspoon-commands")
local chooser = nil
local allChoices = {}
local commandActions = hammerspoonModule.commands

local INITIAL_SELECTION = 2
local SUBTEXT_PENALTY = 50

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

  local results = fuzzy.fuzzy_find(items, query, #choices, SUBTEXT_PENALTY)

  local sortedChoices = {}
  for _, result in ipairs(results) do
    table.insert(sortedChoices, result.original)
  end
  return sortedChoices
end

local function showSwitcher()
  local choices = dispatch.getAllChoices()
  allChoices = choices

  if not chooser then
    chooser = hs.chooser.new(function(choice)
      if choice then
        if choice.window then
          choice.window:focus()
        elseif choice.appName then
          hs.application.launchOrFocus(choice.appName)
        elseif choice.commandId then
          local action = commandActions[choice.commandId]
          if action then
            action()
          end
        end
      end
    end)

    chooser:bgDark(true)
    chooser:fgColor({white = 1.0})
    chooser:subTextColor({white = 0.6})
    chooser:width(50)
    chooser:rows(15)
    chooser:searchSubText(true)

    chooser:queryChangedCallback(function(query)
      if query == "" then
        chooser:choices(allChoices)
      else
        chooser:choices(filterAndSort(allChoices, query))
      end
    end)
  end

  chooser:choices(choices)
  chooser:show()
  chooser:selectedRow(INITIAL_SELECTION)
end

function WindowSwitcher.setup(hyper)
  hyper:bind("tab"):toFunction("Window switcher", showSwitcher)
end

return WindowSwitcher
