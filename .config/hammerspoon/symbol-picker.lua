-- luacheck ignore: hammerspoon runtime
-- ast-grep ignore: hammerspoon runtime
--check:false
local M = {}

package.path = os.getenv("HOME") .. "/lib/?.lua;" .. package.path

local symbols = require("symbols.symbols")
local pickerUtils = require("picker-utils")

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

local function createFilteredSymbols()
  local filtered = {}
  for _, item in ipairs(symbols) do
    if not EXCLUDED_BLOCKS[item.block] then
      table.insert(filtered, item)
    end
  end
  return filtered
end

local picker = pickerUtils.createCachedPicker(createFilteredSymbols(), function(item)
  return {
    text = item.symbol .. "  " .. item.name,
    subText = item.block,
    symbol = item.symbol
  }
end)

M.getSymbolChoices = picker.getChoices
M.clearCache = picker.clearCache
M.insertSymbol = pickerUtils.insertText

return M
