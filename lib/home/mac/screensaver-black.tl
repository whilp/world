local mac = require("mac")

local function run(): number
	local saver_path = "/System/Library/Screen Savers/Basic.saver"
	mac.defaults_current_host("write", "com.apple.screensaver", "modulePath", "-string", saver_path)
	mac.defaults_current_host("write", "com.apple.screensaver", "moduleName", "-string", "Basic")
	mac.defaults_current_host("write", "com.apple.screensaver.Basic", "COLOR", "-string", "0 0 0")
	return 0
end

return { run = run }
