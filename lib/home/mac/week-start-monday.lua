local mac = require("mac")
-- teal ignore: type annotations needed

local function run()
	mac.defaults("write", "NSGlobalDomain", "AppleFirstWeekday", "-dict", "gregorian", "2")
	return 0
end


return { run = run }
