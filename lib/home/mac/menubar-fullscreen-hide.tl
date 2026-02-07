--check:false
local mac = require("mac")

local function run(): number
	mac.defaults("write", "NSGlobalDomain", "AppleMenuBarVisibleInFullscreen", "-bool", "false")
	return 0
end

return { run = run }
