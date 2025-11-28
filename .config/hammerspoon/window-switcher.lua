local WindowSwitcher = {}

local chooser = nil

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

    if not chooser then
      chooser = hs.chooser.new(function(choice)
        if choice then
          if choice.window then
            choice.window:focus()
          elseif choice.appName then
            hs.application.launchOrFocus(choice.appName)
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
