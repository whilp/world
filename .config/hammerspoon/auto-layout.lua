local M = {}

M.config = {
  appMapping = {
    ["zoom.us"] = "center_top",
    ["Google Calendar"] = "center_top",
    ["Gmail"] = "center_top",
    ["Slack"] = "center_bottom",
    ["Google Chrome"] = "main_right",
    ["Ghostty"] = "main_left"
  },

  debounceTime = 5,
  animationDuration = 0.3,
  centerColumnWidth = 0.25
}

local lastPlacement = {}
local windowFilter = nil
local screenWatcher = nil

local function getExternalMonitor()
  local screens = hs.screen.allScreens()
  for _, screen in ipairs(screens) do
    if not screen:name():match("Built%-in") then
      return screen
    end
  end
  return nil
end

local screenMargins = {
  ['DELL U3419W'] = {top = 30, right = 0, bottom = 0, left = 0}
}

local function getUsableFrame(screen)
  local frame = screen:frame()
  local screenName = screen:name()
  local margins = screenMargins[screenName]

  if margins then
    return {
      x = frame.x + margins.left,
      y = frame.y + margins.top,
      w = frame.w - margins.left - margins.right,
      h = frame.h - margins.top - margins.bottom
    }
  end

  return frame
end

local function calculateZones(screen)
  local frame = getUsableFrame(screen)
  local centerWidth = M.config.centerColumnWidth
  local sideWidth = (1 - centerWidth) / 2

  return {
    main_left = {
      x = frame.x,
      y = frame.y,
      w = frame.w * sideWidth,
      h = frame.h
    },
    center_top = {
      x = frame.x + frame.w * sideWidth,
      y = frame.y,
      w = frame.w * centerWidth,
      h = frame.h / 2
    },
    center_bottom = {
      x = frame.x + frame.w * sideWidth,
      y = frame.y + frame.h / 2,
      w = frame.w * centerWidth,
      h = frame.h / 2
    },
    main_right = {
      x = frame.x + frame.w * (sideWidth + centerWidth),
      y = frame.y,
      w = frame.w * sideWidth,
      h = frame.h
    }
  }
end

local function placeWindow(win)
  local t0 = hs.timer.secondsSinceEpoch()
  print(string.format("[auto-layout] placeWindow started at %.3f", t0))

  if not win then
    print("[auto-layout] no window, returning")
    return
  end

  local appName = win:application():name()
  local zoneName = M.config.appMapping[appName]

  if not zoneName then
    print(string.format("[auto-layout] no zone mapping for %s", appName))
    return
  end

  local winId = win:id()
  local now = os.time()
  if lastPlacement[winId] and (now - lastPlacement[winId]) < M.config.debounceTime then
    print(string.format("[auto-layout] debouncing window %d", winId))
    return
  end

  local externalScreen = getExternalMonitor()
  if not externalScreen then
    print("[auto-layout] no external monitor")
    return
  end

  local zones = calculateZones(externalScreen)
  local targetZone = zones[zoneName]

  if targetZone then
    print(string.format("[auto-layout] placing %s in %s", appName, zoneName))
    win:setFrame(targetZone, 0)
    lastPlacement[winId] = now
  end

  local t1 = hs.timer.secondsSinceEpoch()
  print(string.format("[auto-layout] placeWindow completed at %.3f (delta: %.3fms)", t1, (t1-t0)*1000))
end

M.applyLayoutToAllWindows = function()
  local externalScreen = getExternalMonitor()
  if not externalScreen then
    hs.alert.show("No external monitor detected")
    return
  end

  local zones = calculateZones(externalScreen)
  local placed = 0

  for _, win in ipairs(hs.window.allWindows()) do
    local appName = win:application():name()
    local zoneName = M.config.appMapping[appName]

    if zoneName and zones[zoneName] then
      win:setFrame(zones[zoneName], M.config.animationDuration)
      placed = placed + 1
    end
  end

  hs.alert.show(string.format("Placed %d windows", placed))
end

M.setup = function(hyperKey)
  windowFilter = hs.window.filter.new()
  windowFilter:subscribe({
    hs.window.filter.windowCreated,
    hs.window.filter.windowFocused,
    hs.window.filter.windowVisible
  }, function(win)
    placeWindow(win)
  end)

  screenWatcher = hs.screen.watcher.new(function()
    M.applyLayoutToAllWindows()
  end)
  screenWatcher:start()

  hyperKey:bind("w"):toFunction("Apply window layout", function()
    M.applyLayoutToAllWindows()
  end)
end

M.stop = function()
  if windowFilter then windowFilter:unsubscribeAll() end
  if screenWatcher then screenWatcher:stop() end
end

return M
