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

      if kind == 1 and ok and appPath and appPath:match("%.app/?$") then
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

M.getInstalledAppChoices = function(seenApps)
  local choices = {}
  local seenNames = {}

  -- Copy seenApps to seenNames
  for name, _ in pairs(seenApps) do
    seenNames[name] = true
  end

  local appDirs = {
    "/Applications",
    "/System/Applications",
    os.getenv("HOME") .. "/Applications"
  }

  for _, dir in ipairs(appDirs) do
    local iter, dirObj = hs.fs.dir(dir)
    if iter then
      for file in iter, dirObj do
        if file:match("%.app$") then
          local appName = file:gsub("%.app$", "")
          if not seenNames[appName] then
            table.insert(choices, {
              text = appName,
              subText = "Launch application",
              appName = appName
            })
            seenNames[appName] = true
          end
        end
      end
    end
  end

  table.sort(choices, function(a, b)
    return a.text < b.text
  end)

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

  local installedAppChoices = M.getInstalledAppChoices(seenApps)
  for _, choice in ipairs(installedAppChoices) do
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
  elseif item.appName then
    if item.subText == "Focus application" then
      return "running_app"
    else
      return "installed_app"
    end
  elseif item.commandId then return "command"
  elseif item.emoji then return "emoji"
  elseif item.symbol then return "symbol"
  else return "unknown"
  end
end

return M
