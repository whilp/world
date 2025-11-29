local HyperKey = require("hyper-key")
local windowSwitcher = require("window-switcher")
local notchClock = require("notch-clock")
local clueLoader = require("clue-loader")
local clueManager = require("clue-manager")
local emojiPicker = require("emoji-picker")
local symbolPicker = require("symbol-picker")
local chooserStyle = require("chooser-style")

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
})

windowSwitcher.setup(hyper)
notchClock.start()

hs.alert.show("Hammerspoon loaded")
