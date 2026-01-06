local mac = require("mac")
-- teal ignore: type annotations needed

local function run()
	mac.defaults("write", "com.apple.finder", "FinderSpawnTab", "-bool", "false")
	return 0
end


return { run = run }
