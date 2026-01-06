local spawn = require("cosmic.spawn")
-- teal ignore: type annotations needed

local M = {}

function M.defaults(...)
	return spawn({"defaults", ...}):wait() == 0
end

function M.defaults_current_host(...)
	return spawn({"defaults", "-currentHost", ...}):wait() == 0
end

function M.killall(name)
	spawn({"killall", name}):wait()
end

function M.osascript(script)
	return spawn({"osascript", "-e", script}):wait() == 0
end

function M.run_script(name)
	local ok, mod = pcall(require, "mac." .. name)
	if not ok then
		io.stderr:write("error: failed to load mac/" .. name .. "\n")
		return 1
	end
	return mod.run()
end

function M.list()
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

function M.run_all()
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
