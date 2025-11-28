local WindowSwitcher = {}

function WindowSwitcher.setup(hyper)
  local windowFilter = hs.window.filter.new()
  windowFilter:setDefaultFilter({})
  windowFilter:setCurrentSpace(nil)
  windowFilter:setScreens(nil)

  local switcher = hs.window.switcher.new(windowFilter, {
    showTitles = true,
    showThumbnails = true,
    showSelectedThumbnail = true,
    backgroundColor = {0, 0, 0, 0.8},
    highlightColor = {0.3, 0.3, 0.3, 0.8},
    textColor = {1, 1, 1},
    fontName = 'SF Pro',
    fontSize = 16,
    showSelectedTitle = true,
  })

  hyper:bind("tab"):toFunction("Switch windows", function()
    switcher:next()
  end)

  hyper:bind("`"):toFunction("Switch windows backward", function()
    switcher:previous()
  end)
end

return WindowSwitcher
