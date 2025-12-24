local mac = require("mac")

local function run()
	local plist = "<dict><key>enabled</key><false/></dict>"
	mac.defaults("write", "com.apple.symbolichotkeys", "AppleSymbolicHotKeys", "-dict-add", "61", plist)
	print("cmd-tab disabled (log out and back in for changes)")
	return 0
end


return { run = run }
