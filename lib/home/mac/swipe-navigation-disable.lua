local mac = require("mac")
-- teal ignore: type annotations needed

local function run()
	mac.defaults("write", "NSGlobalDomain", "AppleEnableSwipeNavigateWithScrolls", "-bool", "false")
	return 0
end


return { run = run }
