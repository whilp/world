local NotchClock = {}

local clock = nil
local updateTimer = nil
local timeOffsetMinutes = 0

local function updateClockText()
	if clock then
		local adjustedTime = os.time() + (timeOffsetMinutes * 60)
		clock[2].text = os.date("%a %b %d  %H:%M", adjustedTime)
	end
end

local function getClockPosition()
	local clockWidth = 130

	return {
		x = 10,
		y = 6,
		w = clockWidth,
		h = 24,
	}
end

local function createClock()
	if clock then
		clock:delete()
	end

	local pos = getClockPosition()
	clock = hs.canvas.new(pos)

	clock[1] = {
		type = "rectangle",
		fillColor = { red = 0, green = 0, blue = 0, alpha = 0.0 },
		action = "fill",
	}

	local adjustedTime = os.time() + (timeOffsetMinutes * 60)
	clock[2] = {
		type = "text",
		text = os.date("%a %b %d  %H:%M", adjustedTime),
		textColor = { white = 0.85 },
		textSize = 13,
		textFont = ".AppleSystemUIFont",
		textAlignment = "center",
		frame = { x = 0, y = 0, w = pos.w, h = pos.h },
	}

	clock:level("desktop")
	clock:clickActivating(false)
	clock:alpha(1)
	clock:show()
end

local function handleScreenChange()
	createClock()
end

function NotchClock.start(options)
	options = options or {}
	timeOffsetMinutes = options.offsetMinutes or 0
	createClock()

	if updateTimer then
		updateTimer:stop()
	end

	updateTimer = hs.timer.doEvery(60, updateClockText)

	hs.screen.watcher.new(handleScreenChange):start()
end

function NotchClock.stop()
	if clock then
		clock:delete()
		clock = nil
	end

	if updateTimer then
		updateTimer:stop()
		updateTimer = nil
	end
end

return NotchClock
