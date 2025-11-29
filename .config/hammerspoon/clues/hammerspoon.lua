Clue{
  name = "Reload config",
  desc = "Reload Hammerspoon configuration",
  key = { "hyper", "h", "r" },
  group = "hammerspoon",
  show_in_chooser = true,
  action = { fn = function() hs.reload() end }
}

Clue{
  name = "Console",
  desc = "Open Hammerspoon console",
  key = { "hyper", "h", "c" },
  group = "hammerspoon",
  show_in_chooser = true,
  action = { fn = function() hs.openConsole() end }
}

Clue{
  name = "Emoji picker",
  desc = "Open emoji picker",
  key = { "hyper", "h", "e" },
  group = "hammerspoon",
  show_in_chooser = true,
  action = { mode = "emoji" }
}

Clue{
  name = "Symbol picker",
  desc = "Open symbol picker",
  key = { "hyper", "h", "s" },
  group = "hammerspoon",
  show_in_chooser = true,
  action = { mode = "symbol" }
}

Clue{
  name = "Update apps",
  desc = "Run mscupdate to update applications",
  key = { "hyper", "h", "u" },
  group = "hammerspoon",
  show_in_chooser = true,
  action = { shell = "/usr/local/bin/mscupdate" }
}
