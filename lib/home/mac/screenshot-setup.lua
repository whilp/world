local mac = require("mac")
-- teal ignore: type annotations needed

local function run()
	mac.defaults("write", "com.apple.screencapture", "showsClicks", "-bool", "true")
	mac.defaults("write", "com.apple.screencapture", "showsCursor", "-bool", "true")
	mac.defaults("write", "com.apple.screencapture", "location", "-string", "~/Downloads")
	return 0
end


return { run = run }
