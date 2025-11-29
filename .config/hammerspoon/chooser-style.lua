local M = {}

M.apply = function(chooser)
  chooser:bgDark(true)
  chooser:fgColor({white = 1.0})
  chooser:subTextColor({white = 0.6})
  chooser:width(50)
  chooser:rows(15)
  chooser:searchSubText(true)
  chooser:font("Menlo")
end

return M
