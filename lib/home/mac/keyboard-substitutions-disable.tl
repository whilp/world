--check:false
local mac = require("mac")

local function run(): number
	mac.defaults("write", "NSGlobalDomain", "NSAutomaticDashSubstitutionEnabled", "-bool", "false")
	mac.defaults("write", "NSGlobalDomain", "NSAutomaticQuoteSubstitutionEnabled", "-bool", "false")
	mac.defaults("write", "NSGlobalDomain", "NSAutomaticSpellingCorrectionEnabled", "-bool", "false")
	return 0
end

return { run = run }
