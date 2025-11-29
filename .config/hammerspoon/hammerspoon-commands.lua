local commands = {
  reload = function() hs.reload() end,
  console = function() hs.openConsole() end,
  mscupdate = function() hs.execute("/usr/local/bin/mscupdate") end,
  systemsettings = function() hs.execute('open "x-apple.systempreferences:"') end,
  accessibility = function() hs.execute('open "x-apple.systempreferences:com.apple.Accessibility-Settings.extension"') end,
  security = function() hs.execute('open "x-apple.systempreferences:com.apple.preference.security"') end,
  keyboard = function() hs.execute('open "x-apple.systempreferences:com.apple.preference.keyboard"') end,
  displays = function() hs.execute('open "x-apple.systempreferences:com.apple.preference.displays"') end,
  sound = function() hs.execute('open "x-apple.systempreferences:com.apple.preference.sound"') end,
  network = function() hs.execute('open "x-apple.systempreferences:com.apple.Network-Settings.extension"') end,
  general = function() hs.execute('open "x-apple.systempreferences:com.apple.systempreferences.GeneralSettings"') end,
  appearance = function() hs.execute('open "x-apple.systempreferences:com.apple.Appearance-Settings.extension"') end,
  desktop = function() hs.execute('open "x-apple.systempreferences:com.apple.Desktop-Settings.extension"') end,
  notifications = function() hs.execute('open "x-apple.systempreferences:com.apple.preference.notifications"') end,
  trackpad = function() hs.execute('open "x-apple.systempreferences:com.apple.preference.trackpad"') end,
  battery = function() hs.execute('open "x-apple.systempreferences:com.apple.preference.battery"') end,
}

local choices = {
  { text = "Reload config", subText = "Hammerspoon", commandId = "reload" },
  { text = "Console", subText = "Hammerspoon", commandId = "console" },
  { text = "Update apps", subText = "mscupdate", commandId = "mscupdate" },
  { text = "System Settings", subText = "System", commandId = "systemsettings" },
  { text = "Accessibility", subText = "System", commandId = "accessibility" },
  { text = "Privacy & Security", subText = "System", commandId = "security" },
  { text = "Keyboard", subText = "System", commandId = "keyboard" },
  { text = "Displays", subText = "System", commandId = "displays" },
  { text = "Sound", subText = "System", commandId = "sound" },
  { text = "Network", subText = "System", commandId = "network" },
  { text = "General", subText = "System", commandId = "general" },
  { text = "Appearance", subText = "System", commandId = "appearance" },
  { text = "Desktop & Dock", subText = "System", commandId = "desktop" },
  { text = "Notifications", subText = "System", commandId = "notifications" },
  { text = "Trackpad", subText = "System", commandId = "trackpad" },
  { text = "Battery", subText = "System", commandId = "battery" },
}

return {
  choices = choices,
  commands = commands,
}
