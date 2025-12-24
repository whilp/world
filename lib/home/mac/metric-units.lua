local mac = require("mac")

local function run()
	mac.defaults("write", "NSGlobalDomain", "AppleMetricUnits", "-bool", "true")
	mac.defaults("write", "NSGlobalDomain", "AppleMeasurementUnits", "-string", "Centimeters")
	mac.defaults("write", "NSGlobalDomain", "AppleTemperatureUnit", "-string", "Celsius")
	return 0
end


return { run = run }
