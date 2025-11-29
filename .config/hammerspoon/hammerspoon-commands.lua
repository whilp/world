local commands = {
  reload = function() hs.reload() end,
  console = function() hs.openConsole() end,
}

local choices = {
  { text = "Reload config", subText = "Hammerspoon", commandId = "reload" },
  { text = "Console", subText = "Hammerspoon", commandId = "console" },
}

return {
  choices = choices,
  commands = commands,
}
