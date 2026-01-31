local unix = require("cosmo.unix")
local spawn = require("cosmic.spawn")

local record M
	defaults: function(...: string): boolean
	defaults_current_host: function(...: string): boolean
	killall: function(name: string)
	osascript: function(script: string): boolean
	run_script: function(name: string): number
	list: function(): {string}
	run_all: function(): number
end

function M.defaults(...: string): boolean
	return spawn({"defaults", ...}, {env = unix.environ()}):wait() == 0
end

function M.defaults_current_host(...: string): boolean
	return spawn({"defaults", "-currentHost", ...}, {env = unix.environ()}):wait() == 0
end

function M.killall(name: string)
	spawn({"killall", name}, {env = unix.environ()}):wait()
end

function M.osascript(script: string): boolean
	return spawn({"osascript", "-e", script}, {env = unix.environ()}):wait() == 0
end

local record MacModule
	run: function(): number
end

function M.run_script(name: string): number
	local ok, mod = pcall(require, "mac." .. name) as (boolean, MacModule)
	if not ok then
		io.stderr:write("error: failed to load mac/" .. name .. "\n")
		return 1
	end
	return mod.run()
end

function M.list(): {string}
	return {
		"app-switcher-disable",
		"badges-disable",
		"clock-show-date",
		"date-format-iso",
		"desktop-tinting-reduce",
		"dock-gestures-disable",
		"dock-hide",
		"dock-launch-animation-disable",
		"dock-recents-hide",
		"finder-quit-always-keeps-windows",
		"finder-spawn-tab-disable",
		"force-click-disable",
		"keyboard-substitutions-disable",
		"menubar-fullscreen-hide",
		"menubar-hide",
		"metric-units",
		"mouse-tracking-speed",
		"screensaver-black",
		"screenshot-setup",
		"sound-disable",
		"spring-loading-enable",
		"swipe-navigation-disable",
		"tiling-disable",
		"time-24hour",
		"trackpad-gestures-disable",
		"trackpad-right-click-disable",
		"wallpaper",
		"week-start-monday",
		"window-animations-disable",
	}
end

function M.run_all(): number
	for _, name in ipairs(M.list()) do
		local status = M.run_script(name)
		if status ~= 0 then
			return status
		end
	end
	M.killall("Dock")
	M.killall("SystemUIServer")
	return 0
end

return M
