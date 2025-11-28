local HyperKey = require("hyper-key")
local configWatcher = require("config-watch")
local windowSwitcher = require("window-switcher")

hs.ipc.cliInstall()

local hyper = HyperKey.new({"cmd", "ctrl", "alt", "shift"})

windowSwitcher.setup(hyper)

hs.alert.show("Hammerspoon loaded")
