local M = {}

package.path = os.getenv("HOME") .. "/.local/lib/lua/?.lua;" .. package.path

local emoji = require("emoji.emoji")

M.getEmojiChoices = function()
  local choices = {}

  for _, item in ipairs(emoji) do
    table.insert(choices, {
      text = item.emoji .. "  " .. item.name,
      subText = item.group,
      emoji = item.emoji
    })
  end

  return choices
end

M.insertEmoji = function(emoji)
  hs.eventtap.keyStrokes(emoji)
end

return M
