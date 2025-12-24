local mac = require("mac")

local function run()
	mac.defaults("write", "com.apple.dock", "showDesktopGestureEnabled", "-bool", "false")
	mac.defaults("write", "com.apple.dock", "showLaunchpadGestureEnabled", "-bool", "false")
	mac.killall("Dock")
	return 0
end


return { run = run }
