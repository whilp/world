local cosmo = require("cosmo")
local unix = cosmo.unix

local function run(env)
	unix.makedirs(env.SHELLINIT)
	unix.chdir(env.DST)

	local files = {"bashrc", "bash_profile", "profile", "zshrc"}
	for _, name in ipairs(files) do
		local src_path = "." .. name
		local dst_path = env.SHELLINIT .. "/" .. name
		local src_stat = unix.stat(src_path)
		local dst_stat = unix.stat(dst_path)

		if src_stat and not dst_stat then
			local src_fd = unix.open(src_path, unix.O_RDONLY)
			if src_fd and src_fd >= 0 then
				local data = unix.read(src_fd, 1048576)
				unix.close(src_fd)

				if data then
					local dst_fd = unix.open(dst_path, unix.O_WRONLY | unix.O_CREAT | unix.O_TRUNC, 0644)
					if dst_fd and dst_fd >= 0 then
						unix.write(dst_fd, data)
						unix.close(dst_fd)
					end
				end
			end
		end
	end

	return 0
end

return {
	run = run,
}
