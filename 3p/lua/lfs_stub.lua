local cosmo = require("cosmo")
local unix = cosmo.unix

local lfs = {
	_VERSION = "LuaFileSystem 1.8.0 (stub)"
}

function lfs.attributes(filepath, aname)
	local st = unix.stat(filepath)
	if not st then
		return nil, "cannot obtain information from file `" .. filepath .. "'"
	end

	local mode = st:mode()
	local filetype
	if (mode & 0xF000) == 0x4000 then
		filetype = "directory"
	elseif (mode & 0xF000) == 0x8000 then
		filetype = "file"
	else
		filetype = "other"
	end

	local attrs = {
		mode = filetype,
		size = st:size(),
		modification = st:mtim(),
	}

	if aname then
		return attrs[aname]
	end
	return attrs
end

function lfs.currentdir()
	return unix.getcwd()
end

function lfs.dir(path)
	local dir, err = unix.opendir(path)
	if not dir then
		error("cannot open " .. path .. ": " .. (err or "unknown error"))
	end

	return function()
		local entry = dir:read()
		if entry then
			return entry:name()
		end
		return nil
	end
end

function lfs.mkdir(dirname)
	local ok, err = unix.mkdir(dirname, 448)
	if not ok then
		return nil, err
	end
	return true
end

return lfs
