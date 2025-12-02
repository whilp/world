local M = {}

M.config = {
  appMapping = {
    ["zoom.us"] = "top_left",
    ["Slack"] = "bottom_left",
    ["Google Chrome"] = "main_right",
    ["Ghostty"] = "main_right"
  },

  debounceTime = 5,
  animationDuration = 0.3
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

local function calculateZones(screen)
  local frame = screen:frame()

  return {
    top_left = {
      x = frame.x,
      y = frame.y,
      w = frame.w / 3,
      h = frame.h / 2
    },
    bottom_left = {
      x = frame.x,
      y = frame.y + frame.h / 2,
      w = frame.w / 3,
      h = frame.h / 2
    },
    main_right = {
      x = frame.x + frame.w / 3,
      y = frame.y,
      w = frame.w * 2 / 3,
      h = frame.h
    }
  }
end

local function placeWindow(win)
  if not win then return end

  local appName = win:application():name()
  local zoneName = M.config.appMapping[appName]

  if not zoneName then return end

  local winId = win:id()
  local now = os.time()
  if lastPlacement[winId] and (now - lastPlacement[winId]) < M.config.debounceTime then
    return
  end

  local externalScreen = getExternalMonitor()
  if not externalScreen then return end

  local zones = calculateZones(externalScreen)
  local targetZone = zones[zoneName]

  if targetZone then
    win:setFrame(targetZone, 0)
    lastPlacement[winId] = now
  end
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
