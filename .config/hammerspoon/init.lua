local HyperKey = require("hyper-key")
local configWatcher = require("config-watch")
local windowHotkeys = require("window-hotkeys")
local quickSwitch = require("quick-switch")
local windowSwitcher = require("window-switcher")

hs.ipc.cliInstall()

local hyper = HyperKey.new({"cmd", "ctrl", "alt", "shift"})

hyper:bind("r"):toFunction("Reload config", function()
  hs.reload()
end)

hyper:bind("t"):toFunction("Test alert", function()
  hs.alert.show("Hyper key works!")
end)

quickSwitch.setup(hyper)
windowSwitcher.setup(hyper)

hs.alert.show("Hammerspoon loaded")
