local HyperKey = require("hyper-key")
local configWatcher = require("config-watch")

local hyper = HyperKey.new({"cmd", "ctrl", "alt", "shift"})
local super = HyperKey.new({"cmd", "ctrl", "alt"})

hyper:bind("h"):toFunction("Reload config", function()
  hs.reload()
end)

hyper:bind("t"):toFunction("Test alert", function()
  hs.alert.show("Hyper key works!")
end)

hs.alert.show("Hammerspoon loaded")
