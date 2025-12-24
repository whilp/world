local mac = require("mac")

local function run()
	mac.defaults("write", "com.apple.dock", "autohide", "-bool", "true")
	mac.killall("Dock")
	return 0
end


return { run = run }
