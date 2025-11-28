local WindowSwitcher = {}

local chooser = nil
local allChoices = {}

local function fuzzyMatch(str, pattern)
  local strLower = str:lower()
  local patternLower = pattern:lower()
  local patternIdx = 1
  local lastMatchIdx = 0
  local score = 0

  for i = 1, #strLower do
    if patternIdx <= #patternLower and strLower:sub(i, i) == patternLower:sub(patternIdx, patternIdx) then
      local gap = i - lastMatchIdx - 1
      score = score + gap
      lastMatchIdx = i
      patternIdx = patternIdx + 1
    end
  end

  if patternIdx > #patternLower then
    score = score + #strLower
    return true, score
  end
  return false, math.huge
end

local cleanshotCommands = {
  { text = "CleanShot: All-in-one", subText = "Open all capture options", url = "cleanshot://all-in-one" },
  { text = "CleanShot: Area capture", subText = "Capture selected area", url = "cleanshot://capture-area" },
  { text = "CleanShot: Fullscreen", subText = "Capture fullscreen", url = "cleanshot://capture-fullscreen" },
  { text = "CleanShot: Window", subText = "Capture window", url = "cleanshot://capture-window" },
  { text = "CleanShot: Scrolling", subText = "Capture scrolling window", url = "cleanshot://scrolling-capture" },
  { text = "CleanShot: Record screen", subText = "Start screen recording", url = "cleanshot://record-screen" },
  { text = "CleanShot: OCR", subText = "Capture text from screen", url = "cleanshot://capture-text" },
  { text = "CleanShot: History", subText = "Open capture history", url = "cleanshot://open-history" },
  { text = "CleanShot: Toggle desktop icons", subText = "Hide/show desktop icons", url = "cleanshot://toggle-desktop-icons" },
}

local function showSwitcher()
  return function()
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

    for _, app in ipairs(hs.application.runningApplications()) do
      local appName = app:name()
      if appName and not seenApps[appName] then
        local ok, appPath = pcall(app.path, app)
        if ok and appPath and (
          appPath:find("^/Applications/") or
          appPath:find("^/System/Applications/") or
          appPath:find("/Applications/[^/]+%.app$")
        ) then
          table.insert(choices, {
            text = appName,
            subText = "Focus application",
            appName = appName
          })
        end
      end
    end

    for _, cmd in ipairs(cleanshotCommands) do
      table.insert(choices, cmd)
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
          local filtered = {}
          for _, choice in ipairs(allChoices) do
            local searchText = choice.text .. " " .. (choice.subText or "")
            local matches, score = fuzzyMatch(searchText, query)
            if matches then
              table.insert(filtered, {choice = choice, score = score})
            end
          end
          table.sort(filtered, function(a, b) return a.score < b.score end)
          local sortedChoices = {}
          for _, item in ipairs(filtered) do
            table.insert(sortedChoices, item.choice)
          end
          chooser:choices(sortedChoices)
        end
      end)
    end

    chooser:choices(choices)
    chooser:show()
    chooser:selectedRow(2)
  end
end

function WindowSwitcher.setup(hyper)
  hyper:bind("tab"):toFunction("Window switcher", showSwitcher())
end

return WindowSwitcher
