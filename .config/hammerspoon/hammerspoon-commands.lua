local commands = {
  reload = function() hs.reload() end,
  console = function() hs.openConsole() end,
  mscupdate = function() hs.execute("/usr/local/bin/mscupdate") end,
}

local choices = {
  { text = "Reload config", subText = "Hammerspoon", commandId = "reload" },
  { text = "Console", subText = "Hammerspoon", commandId = "console" },
  { text = "Update apps", subText = "mscupdate", commandId = "mscupdate" },
}

return {
  choices = choices,
  commands = commands,
}
