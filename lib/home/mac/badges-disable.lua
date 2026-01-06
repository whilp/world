local mac = require("mac")
-- teal ignore: type annotations needed

local function run()
	mac.defaults("write", "com.apple.dock", "show-recents", "-bool", "false")
	mac.defaults("write", "com.apple.dock", "badge-notifications", "-bool", "false")
	mac.killall("Dock")
	return 0
end


return { run = run }
