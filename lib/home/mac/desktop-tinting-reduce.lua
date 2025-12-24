local mac = require("mac")

local function run()
	mac.defaults("write", "NSGlobalDomain", "AppleReduceDesktopTinting", "-bool", "true")
	return 0
end


return { run = run }
