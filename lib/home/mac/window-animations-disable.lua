local mac = require("mac")

local function run()
	mac.defaults("write", "NSGlobalDomain", "NSAutomaticWindowAnimationsEnabled", "-bool", "false")
	return 0
end


return { run = run }
