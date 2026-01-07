-- luacheck ignore: hammerspoon runtime
local M = {}

M.apply = function(chooser)
  local MAX_WIDTH_PX = 1200
  local screen = hs.screen.mainScreen()
  local screenFrame = screen:frame()
  local maxWidthPercent = (MAX_WIDTH_PX / screenFrame.w) * 100
  local widthPercent = math.min(50, maxWidthPercent)

  chooser:bgDark(true)
  chooser:fgColor({white = 1.0})
  chooser:subTextColor({white = 0.6})
  chooser:width(widthPercent)
  chooser:rows(15)
  chooser:searchSubText(true)
  chooser:showImages(false)
  chooser:showShortcuts(false)
end

return M
