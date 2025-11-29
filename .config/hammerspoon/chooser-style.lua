local M = {}

M.font = { name = "Menlo", size = 14 }

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
      styled[k] = hs.styledtext.new(v, { font = M.font })
    elseif k == "subText" and type(v) == "string" then
      styled[k] = hs.styledtext.new(v, { font = M.font })
    else
      styled[k] = v
    end
  end
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
