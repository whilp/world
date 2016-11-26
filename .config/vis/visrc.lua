-- load standard vis module, providing parts of the Lua API
require('vis')

vis.events.start = function()
	-- Your global configuration options e.g.
	-- vis:command('map! normal j gj')
	vis:command('set escdelay 0')
	vis:command('set ai on')
end

vis.events.win_open = function(win)
	-- enable syntax highlighting for known file types
	vis.filetype_detect(win)

	-- Your per window configuration options e.g.
	-- vis:command('set number')
end

local git_syntax = function (file, data)
	name = 'COMMIT_EDITMSG'
	fname = string.sub(file.name, -string.len(name), -1)
	if fname == name then
		return 'diff'
	end
end

vis.ftdetect.customdetectors = {
	git_syntax,
}
