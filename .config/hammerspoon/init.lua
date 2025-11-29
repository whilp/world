local HyperKey = require("hyper-key")
local windowSwitcher = require("window-switcher")
local notchClock = require("notch-clock")
local clueLoader = require("clue-loader")
local clueManager = require("clue-manager")

hs.ipc.cliInstall()

local hyper = HyperKey.new({"cmd", "ctrl", "alt", "shift"})

clueLoader.load_all()
clueManager.setup(clueLoader)

windowSwitcher.setup(hyper)
notchClock.start()

hs.alert.show("Hammerspoon loaded")
