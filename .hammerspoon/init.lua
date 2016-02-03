local prefix = {"ctrl", "cmd"}
local log = hs.logger.new("whilp", "debug")

hs.window.animationDuration = 0

hs.hotkey.bind(prefix, "R", hs.reload)

hs.hotkey.bind(
  prefix, "F",
  function()
    w = hs.window.focusedWindow()
    s = w:screen()
    w:setFrame(s:fullFrame(), 0)
end)

hs.hotkey.bind(
  prefix, "1",
  function()
    hs.application.launchOrFocusByBundleID("org.gnu.emacs")
end)

hs.hotkey.bind(
  prefix, "2",
    function()
    w = hs.fnutils.find(
      hs.window.allWindows(),
      function(w)
        menu = w:application():findMenuItem({"People", "whilp"})
        log.df("%s %s", w:title(), hs.inspect(menu))
        if menu then
          return menu["ticked"]
        end
    end)
    if w then
      w:focus()
    end
end
)

hs.hotkey.bind(
  prefix, "3",
    function()
    w = hs.fnutils.find(
      hs.window.allWindows(),
      function(w)
        menu = w:application():findMenuItem({"People", "simple"})
        if menu then
          return menu["ticked"]
        end
    end)
    if w then
      w:focus()
    end
end
)

hs.alert.show("config loaded")
