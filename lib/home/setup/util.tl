local spawn = require("cosmic.spawn")

local function copy_tree(src: string, dst: string): boolean
	local status = spawn({"cp", "-ra", src, dst}):wait()
	return status == 0
end

return {
	copy_tree = copy_tree,
}
