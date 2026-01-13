-- luacheck ignore: hammerspoon runtime
return Leader("s", "System", {
  Bind("s", "Settings", { shell = 'open "x-apple.systempreferences:"' }),
  Bind("a", "Accessibility", { shell = 'open "x-apple.systempreferences:com.apple.Accessibility-Settings.extension"' }),
  Bind("p", "Privacy & Security", { shell = 'open "x-apple.systempreferences:com.apple.preference.security"' }),
  Bind("k", "Keyboard", { shell = 'open "x-apple.systempreferences:com.apple.preference.keyboard"' }),
  Bind("d", "Displays", { shell = 'open "x-apple.systempreferences:com.apple.preference.displays"' }),
  Bind("o", "Sound", { shell = 'open "x-apple.systempreferences:com.apple.preference.sound"' }),
  Bind("n", "Network", { shell = 'open "x-apple.systempreferences:com.apple.Network-Settings.extension"' }),
  Bind("g", "General", { shell = 'open "x-apple.systempreferences:com.apple.systempreferences.GeneralSettings"' }),
  Bind("l", "Appearance", { shell = 'open "x-apple.systempreferences:com.apple.Appearance-Settings.extension"' }),
  Bind("w", "Desktop & Dock", { shell = 'open "x-apple.systempreferences:com.apple.Desktop-Settings.extension"' }),
  Bind("i", "Notifications", { shell = 'open "x-apple.systempreferences:com.apple.preference.notifications"' }),
  Bind("t", "Trackpad", { shell = 'open "x-apple.systempreferences:com.apple.preference.trackpad"' }),
  Bind("b", "Battery", { shell = 'open "x-apple.systempreferences:com.apple.preference.battery"' }),
})
