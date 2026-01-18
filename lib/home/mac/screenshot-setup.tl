local mac = require("mac")

local function run(): number
	mac.defaults("write", "com.apple.screencapture", "showsClicks", "-bool", "true")
	mac.defaults("write", "com.apple.screencapture", "showsCursor", "-bool", "true")
	mac.defaults("write", "com.apple.screencapture", "location", "-string", "~/Downloads")
	return 0
end

return { run = run }
