local NotchClock = {}

local clock = nil
local updateTimer = nil
local menubarWatcher = nil
local mouseTimer = nil

local function updateClockText()
	if clock then
		clock[2].text = os.date("%a %b %d  %H:%M")
	end
end

local function getClockPosition()
	local screen = hs.screen.mainScreen()
	local fullFrame = screen:fullFrame()
	local clockWidth = 130

	return {
		x = fullFrame.w - clockWidth - 5,
		y = 5,
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

	clock[2] = {
		type = "text",
		text = os.date("%a %b %d  %H:%M"),
		textColor = { white = 0.85 },
		textSize = 13,
		textFont = ".AppleSystemUIFont",
		textAlignment = "center",
		frame = { x = 0, y = 0, w = pos.w, h = pos.h },
	}

	clock:level("overlay")
	clock:clickActivating(false)
	clock:alpha(0)
	clock:show()
end

local function showClock()
	if clock then
		clock:alpha(1)
	end
end

local function hideClock()
	if clock then
		clock:alpha(0)
	end
end

local function checkMenubar()
	local mousePos = hs.mouse.absolutePosition()
	local screen = hs.screen.mainScreen()
	local screenFrame = screen:fullFrame()

	-- Hide if mouse is in top 34 pixels (menubar area)
	if mousePos.y <= 34 then
		hideClock()
		return
	end

	local frontApp = hs.application.frontmostApplication()
	if frontApp then
		local mainWindow = frontApp:mainWindow()
		if mainWindow and mainWindow:isFullScreen() then
			hideClock()
		else
			showClock()
		end
	else
		showClock()
	end
end

local function handleScreenChange()
	createClock()
end

function NotchClock.start()
	createClock()

	if updateTimer then
		updateTimer:stop()
	end

	updateTimer = hs.timer.doEvery(60, updateClockText)

	hs.screen.watcher.new(handleScreenChange):start()

	menubarWatcher = hs.application.watcher.new(function(appName, eventType, app)
		checkMenubar()
	end)
	menubarWatcher:start()

	hs.window.filter.default:subscribe(hs.window.filter.windowFocused, checkMenubar)
	hs.window.filter.default:subscribe(hs.window.filter.windowUnfocused, checkMenubar)

	-- Check mouse position frequently
	if mouseTimer then
		mouseTimer:stop()
	end
	mouseTimer = hs.timer.doEvery(0.1, checkMenubar)

	checkMenubar()
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

	if mouseTimer then
		mouseTimer:stop()
		mouseTimer = nil
	end

	if menubarWatcher then
		menubarWatcher:stop()
		menubarWatcher = nil
	end
end

return NotchClock
