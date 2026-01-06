local mac = require("mac")
-- teal ignore: type annotations needed

local function run()
	mac.defaults("write", "com.apple.dock", "autohide", "-bool", "true")
	mac.killall("Dock")
	return 0
end


return { run = run }
