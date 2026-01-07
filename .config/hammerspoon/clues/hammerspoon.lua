-- luacheck ignore: hammerspoon runtime
return Leader("h", "Hammerspoon", {
  Bind("r", "Reload config", { fn = function() hs.reload() end }),
  Bind("c", "Console", { fn = function() hs.openConsole() end }),
  Bind("u", "Update apps", { shell = "/usr/local/bin/mscupdate" }),
  Bind("s", "Switcher (unfiltered)", { mode = "unfiltered_switcher" }),
})
