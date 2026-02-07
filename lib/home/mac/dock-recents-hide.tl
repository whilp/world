--check:false
local mac = require("mac")

local function run(): number
	mac.defaults("write", "com.apple.dock", "show-recents", "-bool", "false")
	mac.killall("Dock")
	return 0
end

return { run = run }
