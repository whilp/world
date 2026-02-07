--check:false
local mac = require("mac")

local function run(): number
	mac.defaults("write", "NSGlobalDomain", "com.apple.sound.beep.volume", "-float", "0.0")
	mac.defaults("write", "NSGlobalDomain", "com.apple.sound.uiaudio.enabled", "-int", "0")
	mac.defaults("write", "com.apple.systemsound", "com.apple.sound.beep.flash", "-int", "0")
	return 0
end

return { run = run }
