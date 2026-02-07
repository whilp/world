--check:false
local mac = require("mac")

local function run(): number
	mac.defaults("write", "com.apple.AppleMultitouchTrackpad", "TrackpadFiveFingerPinchGesture", "-int", "0")
	mac.defaults("write", "com.apple.AppleMultitouchTrackpad", "TrackpadFourFingerHorizSwipeGesture", "-int", "0")
	mac.defaults("write", "com.apple.AppleMultitouchTrackpad", "TrackpadFourFingerPinchGesture", "-int", "0")
	mac.defaults("write", "com.apple.AppleMultitouchTrackpad", "TrackpadThreeFingerHorizSwipeGesture", "-int", "0")
	mac.defaults("write", "com.apple.AppleMultitouchTrackpad", "TrackpadTwoFingerFromRightEdgeSwipeGesture", "-int", "0")
	return 0
end

return { run = run }
