local spawn = require("cosmic.spawn").spawn

local function copy_tree(src, dst)
	local status = spawn({"cp", "-ra", src, dst}):wait()
	return status == 0
end

return {
	copy_tree = copy_tree,
}
