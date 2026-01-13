local mac = require("mac")

local function run(): number
	mac.defaults("write", "NSGlobalDomain", "com.apple.trackpad.forceClick", "-bool", "false")
	return 0
end

return { run = run }
