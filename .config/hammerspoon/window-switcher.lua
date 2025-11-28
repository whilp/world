local WindowSwitcher = {}

local chooser = nil

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
