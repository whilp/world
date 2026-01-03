--check:false
local HyperKey = require("hyper-key")
local notchClock = require("notch-clock")

-- Global for IPC access (aerospace on-focus-changed callback)
aeroSwitcher = require("aero-switcher")
local leaderModal = require("leader-modal")
local leaderDsl = require("leader-dsl")
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
  chooser:initialSelectedRow(1)
  chooser:choices(choices)
  chooser:query("")
  chooser:show()
end

local function showSymbolChooser()
  local choices = symbolPicker.getSymbolChoices()
  local chooser = hs.chooser.new(function(choice)
    if choice and choice.symbol then
      symbolPicker.insertSymbol(choice.symbol)
    end
  end)

  chooserStyle.apply(chooser)
  chooser:initialSelectedRow(1)
  chooser:choices(choices)
  chooser:query("")
  chooser:show()
end

local clue_dir = hs.configdir .. "/clues"
local iter, dir_obj = hs.fs.dir(clue_dir)
if iter then
  for file in iter, dir_obj do
    if file:match("%.lua$") and file ~= "." and file ~= ".." then
      local env = {
        Leader = leaderDsl.Leader,
        Bind = leaderDsl.Bind,
      }
      setmetatable(env, { __index = _G })

      local f = io.open(clue_dir .. "/" .. file, "r")
      if f then
        local content = f:read("*all")
        f:close()
        local chunk, err = load(content, file, "t", env)
        if chunk then
          local ok, result = pcall(chunk)
          if ok and result and result.type == "leader" then
            leaderDsl.register_root(result)
          end
        end
      end
    end
  end
end

leaderModal.setup(leaderDsl.get_tree(), { timeout_ms = 3000 }, {
  emoji = showEmojiChooser,
  symbol = showSymbolChooser,
  unfiltered_switcher = function() aeroSwitcher.show() end,
})

notchClock.start({ offsetMinutes = 4 })

-- Window switcher bindings
hs.hotkey.bind({"alt"}, "space", function() aeroSwitcher.show() end)
hs.hotkey.bind({"cmd"}, "space", function() aeroSwitcher.show() end)

hs.alert.show("Hammerspoon loaded")
