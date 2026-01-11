local mac = require("mac")

local function run(): number
	mac.defaults("write", "NSGlobalDomain", "AppleFirstWeekday", "-dict", "gregorian", "2")
	return 0
end

return { run = run }
