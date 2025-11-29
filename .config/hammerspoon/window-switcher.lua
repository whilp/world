local WindowSwitcher = {}

local fuzzy = require("fuzzy")
local cleanshotCommands = require("cleanshot-commands")
local hammerspoonModule = require("hammerspoon-commands")
local chooser = nil
local allChoices = {}
local commandActions = hammerspoonModule.commands

local INITIAL_SELECTION = 1
local SUBTEXT_PENALTY = 50

local function filterAndSort(choices, query)
  local items = {}
  for _, choice in ipairs(choices) do
    table.insert(items, {
      text = choice.text,
      subText = choice.subText,
      type = "window",
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

local function getWindowChoices()
  local windows = hs.window.orderedWindows()
  local choices = {}
  local seenApps = {}

  for _, win in ipairs(windows) do
    local app = win:application()
    local appName = app:name()
    local title = win:title()

    if title and title ~= "" then
      table.insert(choices, {
        text = title,
        subText = appName,
        window = win
      })
      seenApps[appName] = true
    end
  end

  return choices, seenApps
end

local function getAppChoices(seenApps)
  local choices = {}

  for _, app in ipairs(hs.application.runningApplications()) do
    local appName = app:name()
    if appName and not seenApps[appName] then
      local kind = app:kind()
      local ok, appPath = pcall(app.path, app)

      if kind == 1 and ok and appPath and appPath:find("/Applications/") then
        table.insert(choices, {
          text = appName,
          subText = "Focus application",
          appName = appName
        })
      end
    end
  end

  return choices
end

local function getCommandChoices()
  local choices = {}

  for _, choice in ipairs(cleanshotCommands) do
    table.insert(choices, choice)
  end

  for _, choice in ipairs(hammerspoonModule.choices) do
    table.insert(choices, choice)
  end

  return choices
end

local function showSwitcher()
  local choices = {}

  local windowChoices, seenApps = getWindowChoices()
  for _, choice in ipairs(windowChoices) do
    table.insert(choices, choice)
  end

  local appChoices = getAppChoices(seenApps)
  for _, choice in ipairs(appChoices) do
    table.insert(choices, choice)
  end

  local commandChoices = getCommandChoices()
  for _, choice in ipairs(commandChoices) do
    table.insert(choices, choice)
  end

  allChoices = choices

  if not chooser then
    chooser = hs.chooser.new(function(choice)
      if choice then
        if choice.window then
          choice.window:focus()
        elseif choice.appName then
          hs.application.launchOrFocus(choice.appName)
        elseif choice.url then
          hs.urlevent.openURL(choice.url)
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
