local M = {}

local clueLoader = require("clue-loader")

M.getWindowChoices = function()
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

M.getAppChoices = function(seenApps)
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

M.getCommandChoices = function()
  return clueLoader.to_choices()
end

M.getAllChoices = function()
  local choices = {}

  local windowChoices, seenApps = M.getWindowChoices()
  for _, choice in ipairs(windowChoices) do
    table.insert(choices, choice)
  end

  local appChoices = M.getAppChoices(seenApps)
  for _, choice in ipairs(appChoices) do
    table.insert(choices, choice)
  end

  local commandChoices = M.getCommandChoices()
  for _, choice in ipairs(commandChoices) do
    table.insert(choices, choice)
  end

  return choices
end

M.detectType = function(item)
  if item.window then return "window"
  elseif item.appName then return "app"
  elseif item.commandId then return "command"
  elseif item.emoji then return "emoji"
  elseif item.symbol then return "symbol"
  else return "unknown"
  end
end

return M
