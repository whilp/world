local M = {}

package.path = os.getenv("HOME") .. "/.local/lib/lua/?.lua;" .. package.path

local emoji = require("emoji.emoji")

local cachedChoices = nil

M.getEmojiChoices = function()
  if cachedChoices then
    return cachedChoices
  end

  local choices = {}

  for _, item in ipairs(emoji) do
    table.insert(choices, {
      text = item.emoji .. "  " .. item.name,
      subText = item.group,
      emoji = item.emoji
    })
  end

  cachedChoices = choices
  return choices
end

M.insertEmoji = function(emoji)
  hs.eventtap.keyStrokes(emoji)
end

return M
