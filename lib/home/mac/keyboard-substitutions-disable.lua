local mac = require("mac")
-- teal ignore: type annotations needed

local function run()
	mac.defaults("write", "NSGlobalDomain", "NSAutomaticDashSubstitutionEnabled", "-bool", "false")
	mac.defaults("write", "NSGlobalDomain", "NSAutomaticQuoteSubstitutionEnabled", "-bool", "false")
	mac.defaults("write", "NSGlobalDomain", "NSAutomaticSpellingCorrectionEnabled", "-bool", "false")
	return 0
end


return { run = run }
