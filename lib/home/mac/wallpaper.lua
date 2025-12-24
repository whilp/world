local mac = require("mac")

local function run()
	mac.osascript([[tell application "System Events" to tell every desktop to set picture to "/System/Library/Desktop Pictures/Solid Colors/Black.png"]])
	return 0
end


return { run = run }
