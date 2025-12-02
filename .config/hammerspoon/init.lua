local HyperKey = require("hyper-key")
local windowSwitcher = require("window-switcher")
local notchClock = require("notch-clock")
local clueLoader = require("clue-loader")
local clueManager = require("clue-manager")
local emojiPicker = require("emoji-picker")
local symbolPicker = require("symbol-picker")
local chooserStyle = require("chooser-style")
local autoLayout = require("auto-layout")

hs.ipc.cliInstall()

local hyper = HyperKey.new({"cmd", "ctrl", "alt", "shift"})

local function showEmojiChooser()
  local choices = emojiPicker.getEmojiChoices()
  local chooser = hs.chooser.new(function(choice)
    if choice and choice.emoji then
      emojiPicker.insertEmoji(choice.emoji)
    end
  end)

  chooserStyle.apply(chooser)
  chooser:choices(choices)
  chooser:query("")
  chooser:show()
  chooser:selectedRow(1)
end

local function showSymbolChooser()
  local choices = symbolPicker.getSymbolChoices()
  local chooser = hs.chooser.new(function(choice)
    if choice and choice.symbol then
      symbolPicker.insertSymbol(choice.symbol)
    end
  end)

  chooserStyle.apply(chooser)
  chooser:choices(choices)
  chooser:query("")
  chooser:show()
  chooser:selectedRow(1)
end

clueLoader.load_all()
clueManager.setup(clueLoader, {
  emoji = showEmojiChooser,
  symbol = showSymbolChooser,
  unfiltered_switcher = function() windowSwitcher.showUnfiltered() end,
})

local metaModal = clueManager.create_meta_modal(hyper)
hyper:bind("space"):toFunction("Show modals", function()
  metaModal:enter()
end)

windowSwitcher.setup(hyper)
notchClock.start({ offsetMinutes = 4 })
autoLayout.setup(hyper)

-- cmd+space opens dispatcher
hs.hotkey.bind({"cmd"}, "space", function()
  windowSwitcher.showUnfiltered()
end)

-- cmd+tab opens dispatcher
hs.hotkey.bind({"cmd"}, "tab", function()
  windowSwitcher.showUnfiltered()
end)

hs.alert.show("Hammerspoon loaded")
