--check:false
local mac = require("mac")

local function run(): number
	mac.defaults("write", "NSGlobalDomain", "AppleICUDateFormatStrings", "-dict", "1", "y-MM-dd")
	return 0
end

return { run = run }
