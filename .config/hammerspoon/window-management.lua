local WindowManagement = {}

-- grid configuration
local gridConfig = {
  normal = {w = 8, h = 4},
  ultrawide = {w = 10, h = 4},
  vertical = {w = 4, h = 8}
}

-- initialize grid with default size
hs.grid.setGrid(gridConfig.normal.w .. 'x' .. gridConfig.normal.h)
hs.grid.setMargins({0, 0})

-- detect screen type and adjust grid
local function adjustGridForScreen(screen)
  local frame = screen:frame()
  local aspectRatio = frame.w / frame.h

  if aspectRatio > 2.5 then
    hs.grid.setGrid(gridConfig.ultrawide.w .. 'x' .. gridConfig.ultrawide.h)
  elseif aspectRatio < 1 then
    hs.grid.setGrid(gridConfig.vertical.w .. 'x' .. gridConfig.vertical.h)
  else
    hs.grid.setGrid(gridConfig.normal.w .. 'x' .. gridConfig.normal.h)
  end
end

-- watch for screen changes
local screenWatcher = hs.screen.watcher.new(function()
  local screen = hs.screen.mainScreen()
  adjustGridForScreen(screen)
end)
screenWatcher:start()

-- initialize grid for current screen
adjustGridForScreen(hs.screen.mainScreen())

-- helper to get current window
local function getWindow()
  return hs.window.focusedWindow()
end

-- configuration for screen-specific margins
local screenMargins = {
  ['DELL U3419W'] = {top = 30, right = 0, bottom = 0, left = 0}
}

-- helper to get usable frame for a screen
-- accounts for menu bar on the screen where it's located
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

-- maximize window
function WindowManagement.maximizeWindow()
  local win = getWindow()
  if not win then return end
  win:maximize()
end

-- center window
function WindowManagement.centerOnScreen()
  local win = getWindow()
  if not win then return end
  win:centerOnScreen()
end

-- half screen layouts
function WindowManagement.leftHalf()
  local win = getWindow()
  if not win then return end
  local screen = win:screen()
  local max = getUsableFrame(screen)
  win:setFrame({
    x = max.x,
    y = max.y,
    w = max.w / 2,
    h = max.h
  })
end

function WindowManagement.rightHalf()
  local win = getWindow()
  if not win then return end
  local screen = win:screen()
  local max = getUsableFrame(screen)
  win:setFrame({
    x = max.x + max.w / 2,
    y = max.y,
    w = max.w / 2,
    h = max.h
  })
end

function WindowManagement.topHalf()
  local win = getWindow()
  if not win then return end
  local screen = win:screen()
  local max = getUsableFrame(screen)
  win:setFrame({
    x = max.x,
    y = max.y,
    w = max.w,
    h = max.h / 2
  })
end

function WindowManagement.bottomHalf()
  local win = getWindow()
  if not win then return end
  local screen = win:screen()
  local max = getUsableFrame(screen)
  win:setFrame({
    x = max.x,
    y = max.y + max.h / 2,
    w = max.w,
    h = max.h / 2
  })
end

-- corner layouts
function WindowManagement.topLeft()
  local win = getWindow()
  if not win then return end
  local screen = win:screen()
  local max = getUsableFrame(screen)
  win:setFrame({
    x = max.x,
    y = max.y,
    w = max.w / 2,
    h = max.h / 2
  })
end

function WindowManagement.topRight()
  local win = getWindow()
  if not win then return end
  local screen = win:screen()
  local max = getUsableFrame(screen)
  win:setFrame({
    x = max.x + max.w / 2,
    y = max.y,
    w = max.w / 2,
    h = max.h / 2
  })
end

function WindowManagement.bottomLeft()
  local win = getWindow()
  if not win then return end
  local screen = win:screen()
  local max = getUsableFrame(screen)
  win:setFrame({
    x = max.x,
    y = max.y + max.h / 2,
    w = max.w / 2,
    h = max.h / 2
  })
end

function WindowManagement.bottomRight()
  local win = getWindow()
  if not win then return end
  local screen = win:screen()
  local max = getUsableFrame(screen)
  win:setFrame({
    x = max.x + max.w / 2,
    y = max.y + max.h / 2,
    w = max.w / 2,
    h = max.h / 2
  })
end

-- thirds
function WindowManagement.leftThird()
  local win = getWindow()
  if not win then return end
  local screen = win:screen()
  local max = getUsableFrame(screen)
  win:setFrame({
    x = max.x,
    y = max.y,
    w = max.w / 3,
    h = max.h
  })
end

function WindowManagement.centerThird()
  local win = getWindow()
  if not win then return end
  local screen = win:screen()
  local max = getUsableFrame(screen)
  win:setFrame({
    x = max.x + max.w / 3,
    y = max.y,
    w = max.w / 3,
    h = max.h
  })
end

function WindowManagement.rightThird()
  local win = getWindow()
  if not win then return end
  local screen = win:screen()
  local max = getUsableFrame(screen)
  win:setFrame({
    x = max.x + (max.w * 2 / 3),
    y = max.y,
    w = max.w / 3,
    h = max.h
  })
end

-- two thirds
function WindowManagement.leftTwoThirds()
  local win = getWindow()
  if not win then return end
  local screen = win:screen()
  local max = getUsableFrame(screen)
  win:setFrame({
    x = max.x,
    y = max.y,
    w = max.w * 2 / 3,
    h = max.h
  })
end

function WindowManagement.rightTwoThirds()
  local win = getWindow()
  if not win then return end
  local screen = win:screen()
  local max = getUsableFrame(screen)
  win:setFrame({
    x = max.x + max.w / 3,
    y = max.y,
    w = max.w * 2 / 3,
    h = max.h
  })
end

-- move between displays
function WindowManagement.throwLeft()
  local win = getWindow()
  if not win then return end
  local screen = win:screen()
  local nextScreen = screen:toWest()
  if nextScreen then
    win:moveToScreen(nextScreen)
  end
end

function WindowManagement.throwRight()
  local win = getWindow()
  if not win then return end
  local screen = win:screen()
  local nextScreen = screen:toEast()
  if nextScreen then
    win:moveToScreen(nextScreen)
  end
end

function WindowManagement.throwUp()
  local win = getWindow()
  if not win then return end
  local screen = win:screen()
  local nextScreen = screen:toNorth()
  if nextScreen then
    win:moveToScreen(nextScreen)
  end
end

function WindowManagement.throwDown()
  local win = getWindow()
  if not win then return end
  local screen = win:screen()
  local nextScreen = screen:toSouth()
  if nextScreen then
    win:moveToScreen(nextScreen)
  end
end

-- resize adjustments
local resizeAmount = 40

function WindowManagement.shrinkLeft()
  local win = getWindow()
  if not win then return end
  local frame = win:frame()
  frame.w = frame.w - resizeAmount
  frame.x = frame.x + resizeAmount
  win:setFrame(frame)
end

function WindowManagement.growRight()
  local win = getWindow()
  if not win then return end
  local frame = win:frame()
  frame.w = frame.w + resizeAmount
  win:setFrame(frame)
end

function WindowManagement.shrinkUp()
  local win = getWindow()
  if not win then return end
  local frame = win:frame()
  frame.h = frame.h - resizeAmount
  frame.y = frame.y + resizeAmount
  win:setFrame(frame)
end

function WindowManagement.growDown()
  local win = getWindow()
  if not win then return end
  local frame = win:frame()
  frame.h = frame.h + resizeAmount
  win:setFrame(frame)
end

-- nudge position
local nudgeAmount = 40

function WindowManagement.nudgeLeft()
  local win = getWindow()
  if not win then return end
  local frame = win:frame()
  frame.x = frame.x - nudgeAmount
  win:setFrame(frame)
end

function WindowManagement.nudgeRight()
  local win = getWindow()
  if not win then return end
  local frame = win:frame()
  frame.x = frame.x + nudgeAmount
  win:setFrame(frame)
end

function WindowManagement.nudgeUp()
  local win = getWindow()
  if not win then return end
  local frame = win:frame()
  frame.y = frame.y - nudgeAmount
  win:setFrame(frame)
end

function WindowManagement.nudgeDown()
  local win = getWindow()
  if not win then return end
  local frame = win:frame()
  frame.y = frame.y + nudgeAmount
  win:setFrame(frame)
end

return WindowManagement
