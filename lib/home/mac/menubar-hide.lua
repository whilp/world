local mac = require("mac")

local function run()
	mac.defaults("write", "NSGlobalDomain", "_HIHideMenuBar", "-bool", "true")
	return 0
end


return { run = run }
