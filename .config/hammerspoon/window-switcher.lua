local WindowSwitcher = {}

local chooser = nil
local cachedChoices = {}

local function getWindowChoices()
  local choices = {}
  local windows = hs.window.filter.default:getWindows(hs.window.filter.sortByFocusedLast)

  for _, win in ipairs(windows) do
    local app = win:application()
    local title = win:title()

    if title and title ~= "" then
      table.insert(choices, {
        text = title,
        subText = app:name(),
        window = win
      })
    end
  end

  return choices
end

local function updateChoices()
  cachedChoices = getWindowChoices()
  if chooser then
    chooser:choices(cachedChoices)
  end
end

local function showChooser()
  if not chooser then
    chooser = hs.chooser.new(function(choice)
      if choice and choice.window then
        choice.window:focus()
      end
    end)

    chooser:bgDark(true)
    chooser:fgColor({white = 1.0})
    chooser:subTextColor({white = 0.6})
    chooser:width(40)
    chooser:rows(12)
    chooser:searchSubText(true)
    chooser:placeholderText("Search windows...")

    cachedChoices = getWindowChoices()
  end

  chooser:choices(cachedChoices)
  chooser:show()

  -- update in background
  hs.timer.doAfter(0, updateChoices)
end

function WindowSwitcher.setup(hyper)
  hyper:bind("tab"):toFunction("Fuzzy window switcher", showChooser)
end

return WindowSwitcher
