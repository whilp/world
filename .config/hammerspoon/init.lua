local HyperKey = require("hyper-key")
local windowSwitcher = require("window-switcher")
local notchClock = require("notch-clock")

hs.ipc.cliInstall()

local hyper = HyperKey.new({"cmd", "ctrl", "alt", "shift"})

windowSwitcher.setup(hyper)
notchClock.start()

hs.alert.show("Hammerspoon loaded")
