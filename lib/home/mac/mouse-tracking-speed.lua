local mac = require("mac")

local function run()
	mac.defaults("write", "NSGlobalDomain", "com.apple.mouse.scaling", "-float", "0.5")
	mac.defaults("write", "NSGlobalDomain", "com.apple.scrollwheel.scaling", "-float", "0.125")
	return 0
end


return { run = run }
