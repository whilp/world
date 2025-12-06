local wm = require("window-management")

-- Track current layout cycle position
local layoutIndex = 1
local layouts = {
  {name = "Left", fn = wm.leftHalf},
  {name = "Right", fn = wm.rightHalf},
  {name = "Top", fn = wm.topHalf},
  {name = "Bottom", fn = wm.bottomHalf},
  {name = "Center", fn = wm.centerOnScreen},
  {name = "Max", fn = wm.maximizeWindow},
}

local function cycleLayout()
  layoutIndex = layoutIndex + 1
  if layoutIndex > #layouts then
    layoutIndex = 1
  end

  local layout = layouts[layoutIndex]
  layout.fn()
  hs.alert.show(layout.name, 0.5)
end

local function throwToNextDisplay()
  local win = hs.window.focusedWindow()
  if not win then return end

  local screen = win:screen()
  local nextScreen = screen:next()

  if nextScreen then
    win:moveToScreen(nextScreen)
    hs.alert.show("→ " .. nextScreen:name(), 0.5)
  end
end

local function throwToPrevDisplay()
  local win = hs.window.focusedWindow()
  if not win then return end

  local screen = win:screen()
  local prevScreen = screen:previous()

  if prevScreen then
    win:moveToScreen(prevScreen)
    hs.alert.show("← " .. prevScreen:name(), 0.5)
  end
end

return Leader("r", "Windows", {
  Bind("h", "Left half", { fn = wm.leftHalf }, { sticky = true }),
  Bind("l", "Right half", { fn = wm.rightHalf }, { sticky = true }),
  Bind("k", "Top half", { fn = wm.topHalf }, { sticky = true }),
  Bind("j", "Bottom half", { fn = wm.bottomHalf }, { sticky = true }),
  Bind("f", "Maximize", { fn = wm.maximizeWindow }, { sticky = true }),
  Bind("c", "Center", { fn = wm.centerOnScreen }, { sticky = true }),
  Bind("n", "Next display", { fn = throwToNextDisplay }, { sticky = true }),
  Bind("p", "Prev display", { fn = throwToPrevDisplay }, { sticky = true }),
  Bind("space", "Cycle layout", { fn = cycleLayout }, { sticky = true }),
})
