local mac = require("mac")
-- teal ignore: type annotations needed

local function run()
	mac.defaults("write", "NSGlobalDomain", "AppleICUForce24HourTime", "-bool", "true")
	return 0
end


return { run = run }
