local M = {}

package.path = os.getenv("HOME") .. "/.local/lib/lua/?.lua;" .. package.path

local symbols = require("symbols.symbols")

local EXCLUDED_BLOCKS = {
  ["Sutton SignWriting"] = true,
  ["Byzantine Musical Symbols"] = true,
  ["Musical Symbols"] = true,
  ["Znamenny Musical Notation"] = true,
  ["Braille Patterns"] = true,
  ["CJK Compatibility"] = true,
  ["CJK Radicals Supplement"] = true,
  ["Kangxi Radicals"] = true,
  ["Enclosed CJK Letters and Months"] = true,
}

local cachedChoices = nil

M.getSymbolChoices = function()
  if cachedChoices then
    return cachedChoices
  end

  local choices = {}

  for _, item in ipairs(symbols) do
    if not EXCLUDED_BLOCKS[item.block] then
      table.insert(choices, {
        text = item.symbol .. "  " .. item.name,
        subText = item.block,
        symbol = item.symbol
      })
    end
  end

  cachedChoices = choices
  return choices
end

M.insertSymbol = function(symbol)
  hs.eventtap.keyStrokes(symbol)
end

return M
