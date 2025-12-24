local mac = require("mac")

local function run()
	local script = [[
tell application "System Events" to tell every desktop
	set picture to "/System/Library/Desktop Pictures/Solid Colors/Black.png"
end tell]]
	mac.osascript(script)
	return 0
end


return { run = run }
