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
  name = "Emoji picker",
  desc = "open emoji picker",
  key = { "hyper", "h", "e" },
  group = "hammerspoon",
  show_in_chooser = true,
  action = { mode = "emoji" }
}

Clue{
  name = "Symbol picker",
  desc = "open symbol picker",
  key = { "hyper", "h", "s" },
  group = "hammerspoon",
  show_in_chooser = true,
  action = { mode = "symbol" }
}

Clue{
  name = "Update apps",
  desc = "run mscupdate to update applications",
  key = { "hyper", "h", "u" },
  group = "hammerspoon",
  show_in_chooser = true,
  action = { shell = "/usr/local/bin/mscupdate" }
}
