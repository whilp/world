local mac = require("mac")

local function run(): number
	mac.defaults("write", "com.apple.finder", "FinderSpawnTab", "-bool", "false")
	return 0
end

return { run = run }
