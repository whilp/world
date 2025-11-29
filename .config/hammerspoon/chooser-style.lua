local M = {}

M.font = { name = "Menlo", size = 14 }
M.maxTextLength = 80

local function truncate(text, maxLen)
  if type(text) ~= "string" then return text end
  if #text <= maxLen then return text end
  return text:sub(1, maxLen - 1) .. "…"
end

M.apply = function(chooser)
  chooser:bgDark(true)
  chooser:fgColor({white = 1.0})
  chooser:subTextColor({white = 0.6})
  chooser:width(50)
  chooser:rows(15)
  chooser:searchSubText(true)
end

M.styleChoice = function(choice)
  local styled = {}
  for k, v in pairs(choice) do
    if k == "text" and type(v) == "string" then
      local truncated = truncate(v, M.maxTextLength)
      styled[k] = hs.styledtext.new(truncated, { font = M.font, paragraphStyle = { lineBreak = "truncateTail" } })
    elseif k == "subText" and type(v) == "string" then
      styled[k] = hs.styledtext.new(v, { font = M.font })
    elseif k ~= "image" and k ~= "valid" then
      styled[k] = v
    end
  end
  styled.image = nil
  styled.valid = nil
  return styled
end

M.styleChoices = function(choices)
  local styled = {}
  for _, choice in ipairs(choices) do
    table.insert(styled, M.styleChoice(choice))
  end
  return styled
end

return M
