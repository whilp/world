local mac = require("mac")
-- teal ignore: type annotations needed

local function run()
	mac.defaults("write", "com.apple.menuextra.clock", "ShowDate", "-int", "1")
	mac.killall("SystemUIServer")
	return 0
end


return { run = run }
