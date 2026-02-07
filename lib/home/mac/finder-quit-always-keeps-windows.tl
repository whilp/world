--check:false
local mac = require("mac")

local function run(): number
	mac.defaults("write", "NSGlobalDomain", "NSQuitAlwaysKeepsWindows", "-bool", "true")
	return 0
end

return { run = run }
