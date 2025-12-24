local mac = require("mac")

local function run()
	mac.defaults("write", "com.apple.dock", "show-recents", "-bool", "false")
	mac.killall("Dock")
	return 0
end


return { run = run }
