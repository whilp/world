local WindowSwitcher = {}

local chooser = nil

local function showSwitcher()
  return function()
    -- get windows immediately
    local windows = hs.window.orderedWindows()
    local choices = {}

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

    if not chooser then
      chooser = hs.chooser.new(function(choice)
        if choice and choice.window then
          choice.window:focus()
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
  end
end

function WindowSwitcher.setup(hyper)
  hyper:bind("tab"):toFunction("Window switcher", showSwitcher())
end

return WindowSwitcher
