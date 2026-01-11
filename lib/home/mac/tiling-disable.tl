local mac = require("mac")

local function run(): number
	mac.defaults("write", "com.apple.WindowManager", "EnableStandardClickToShowDesktop", "-bool", "false")
	return 0
end

return { run = run }
