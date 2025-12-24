local mac = require("mac")

local function run()
	mac.defaults("write", "NSGlobalDomain", "com.apple.springing.enabled", "-bool", "true")
	mac.defaults("write", "NSGlobalDomain", "com.apple.springing.delay", "-float", "0.5")
	return 0
end


return { run = run }
