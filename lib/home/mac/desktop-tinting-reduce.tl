local mac = require("mac")

local function run(): number
	mac.defaults("write", "NSGlobalDomain", "AppleReduceDesktopTinting", "-bool", "true")
	return 0
end

return { run = run }
