--check:false
local mac = require("mac")

local function run(): number
	mac.defaults("write", "com.apple.dock", "launchanim", "-bool", "false")
	mac.killall("Dock")
	return 0
end

return { run = run }
