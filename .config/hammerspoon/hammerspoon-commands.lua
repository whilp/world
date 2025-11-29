local commands = {
  reload = function() hs.reload() end,
  console = function() hs.openConsole() end,
  mscupdate = function() hs.execute("/usr/local/bin/mscupdate") end,
}

local choices = {
  { text = "Reload config", subText = "Hammerspoon", commandId = "reload" },
  { text = "Console", subText = "Hammerspoon", commandId = "console" },
  { text = "Update apps", subText = "mscupdate", commandId = "mscupdate" },
  { text = "System Settings", subText = "System", url = "x-apple.systempreferences:" },
  { text = "Accessibility", subText = "System", url = "x-apple.systempreferences:com.apple.Accessibility-Settings.extension" },
  { text = "Privacy & Security", subText = "System", url = "x-apple.systempreferences:com.apple.preference.security" },
  { text = "Keyboard", subText = "System", url = "x-apple.systempreferences:com.apple.preference.keyboard" },
  { text = "Displays", subText = "System", url = "x-apple.systempreferences:com.apple.preference.displays" },
  { text = "Sound", subText = "System", url = "x-apple.systempreferences:com.apple.preference.sound" },
  { text = "Network", subText = "System", url = "x-apple.systempreferences:com.apple.Network-Settings.extension" },
  { text = "General", subText = "System", url = "x-apple.systempreferences:com.apple.systempreferences.GeneralSettings" },
  { text = "Appearance", subText = "System", url = "x-apple.systempreferences:com.apple.Appearance-Settings.extension" },
  { text = "Desktop & Dock", subText = "System", url = "x-apple.systempreferences:com.apple.Desktop-Settings.extension" },
  { text = "Notifications", subText = "System", url = "x-apple.systempreferences:com.apple.preference.notifications" },
  { text = "Trackpad", subText = "System", url = "x-apple.systempreferences:com.apple.preference.trackpad" },
  { text = "Battery", subText = "System", url = "x-apple.systempreferences:com.apple.preference.battery" },
}

return {
  choices = choices,
  commands = commands,
}
