Clue{
  name = "Reload config",
  desc = "reload configuration",
  key = { "hyper", "h", "r" },
  group = "hammerspoon",
  show_in_chooser = true,
  action = { fn = function() hs.reload() end }
}

Clue{
  name = "Console",
  desc = "open console",
  key = { "hyper", "h", "c" },
  group = "hammerspoon",
  show_in_chooser = true,
  action = { fn = function() hs.openConsole() end }
}

Clue{
  name = "Update apps",
  desc = "run mscupdate to update applications",
  key = { "hyper", "h", "u" },
  group = "hammerspoon",
  show_in_chooser = true,
  action = { shell = "/usr/local/bin/mscupdate" }
}

Clue{
  name = "Switcher (unfiltered)",
  desc = "show all apps without filtering",
  key = { "hyper", "h", "s" },
  group = "hammerspoon",
  show_in_chooser = true,
  action = { mode = "unfiltered_switcher" }
}

Clue{
  name = "Snap layout",
  desc = "apply window layout to external monitor",
  key = { "hyper", "h", "l" },
  group = "hammerspoon",
  show_in_chooser = true,
  action = { fn = function()
    local autoLayout = require("auto-layout")
    autoLayout.applyLayoutToAllWindows()
  end }
}
