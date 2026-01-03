--check:false
local M = {}

package.path = os.getenv("HOME") .. "/lib/?.lua;" .. package.path

local emoji = require("emoji.emoji")
local pickerUtils = require("picker-utils")

local picker = pickerUtils.createCachedPicker(emoji, function(item)
  return {
    text = item.emoji .. "  " .. item.name,
    subText = item.group,
    emoji = item.emoji
  }
end)

M.getEmojiChoices = picker.getChoices
M.clearCache = picker.clearCache
M.insertEmoji = pickerUtils.insertText

return M
