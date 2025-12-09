local M = {}

local leaderDsl = require("leader-dsl")

M.filteredApps = {
  "Chess",
  "Karabiner-VirtualHIDDevice-Manager",
  "Bigmac",
  "Books",
  "Calculator",
  "System Calcluator",
  "Archive Utility",
  "Activity Monitor",
  "Font Book",
  "Dictionary",
  "VoiceOver Utility",
  "Stickies",
  "TextEdit",
}

local function shouldFilterApp(appName)
  for _, filtered in ipairs(M.filteredApps) do
    if appName == filtered or appName:match("^%.") then
      return true
    end
  end
  return false
end

M.getWindowChoices = function(applyFilter)
  local windows = hs.window.orderedWindows()
  local choices = {}
  local seenApps = {}

  for index, win in ipairs(windows) do
    local app = win:application()
    local appName = app:name()
    local title = win:title()

    -- Filter out non-standard windows (utility dialogs, system dialogs, etc.)
    if not win:isStandard() then
      goto continue
    end

    -- Filter out invisible windows
    if not win:isVisible() then
      goto continue
    end

    if (not applyFilter or not shouldFilterApp(appName)) then
      -- Use app name as title if window title is empty (common for Chrome apps/PWAs)
      local displayTitle = (title and title ~= "") and title or appName
      local subText = (title and title ~= "") and appName or "Application window"

      -- Extract Chrome profile from title
      if appName == "Google Chrome" then
        local profile = title:match(" %- Google Chrome %- (.+)$")
        if profile then
          displayTitle = title:gsub(" %- Google Chrome %- .+$", "")
          subText = appName .. " - " .. profile
        end
      end

      table.insert(choices, {
        text = displayTitle,
        subText = subText,
        window = win,
        mruIndex = index  -- Store MRU position for scoring
      })
      seenApps[appName] = true
    end
    ::continue::
  end

  return choices, seenApps
end

M.getAppChoices = function(seenApps, applyFilter)
  local choices = {}

  for _, app in ipairs(hs.application.runningApplications()) do
    local appName = app:name()
    if appName and not seenApps[appName] and (not applyFilter or not shouldFilterApp(appName)) then
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

M.getInstalledAppChoices = function(seenApps, applyFilter)
  local choices = {}
  local seenNames = {}

  -- Copy seenApps to seenNames
  for name, _ in pairs(seenApps) do
    seenNames[name] = true
  end

  local appDirs = {
    "/Applications",
    "/System/Applications",
    os.getenv("HOME") .. "/Applications",
    os.getenv("HOME") .. "/Applications/Chrome Apps.localized"
  }

  for _, dir in ipairs(appDirs) do
    local iter, dirObj = hs.fs.dir(dir)
    if iter then
      for file in iter, dirObj do
        if file:match("%.app$") then
          local appName = file:gsub("%.app$", "")
          if not seenNames[appName] and (not applyFilter or not shouldFilterApp(appName)) then
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
  local choices = {}
  local tree = leaderDsl.get_tree()

  local function traverse(node, prefix, group)
    for key, child in pairs(node) do
      if child.type == "bind" then
        local commandId = prefix .. key
        table.insert(choices, {
          text = child.desc,
          subText = group,
          commandId = commandId
        })
      elseif child.type == "leader" then
        traverse(child.children, prefix .. key .. ".", child.desc)
      end
    end
  end

  traverse(tree, "", "")
  return choices
end

M.getCommandAction = function(commandId)
  local tree = leaderDsl.get_tree()
  local parts = {}
  for part in commandId:gmatch("[^.]+") do
    table.insert(parts, part)
  end

  local node = tree
  for i, part in ipairs(parts) do
    if not node or not node[part] then
      return nil
    end
    node = node[part]
    if i < #parts then
      node = node.children
    end
  end

  return node and node.action
end

M.getAllChoices = function(applyFilter)
  local choices = {}

  local windowChoices, seenApps = M.getWindowChoices(applyFilter)
  for _, choice in ipairs(windowChoices) do
    table.insert(choices, choice)
  end

  local appChoices = M.getAppChoices(seenApps, applyFilter)
  for _, choice in ipairs(appChoices) do
    table.insert(choices, choice)
  end

  local installedAppChoices = M.getInstalledAppChoices(seenApps, applyFilter)
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
