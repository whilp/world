local cosmo = require("cosmo") -- luacheck: ignore

local function main()
	print("validating setup scripts...")
	print()

	local env_module = require("setup.env")
	local env = env_module.get()

	print("environment:")
	print(string.format("  SRC: %s", env.SRC))
	print(string.format("  DST: %s", env.DST))
	print(string.format("  SHELLINIT: %s", env.SHELLINIT))
	print(string.format("  REMOTE: %s", env.REMOTE))
	print()

	print("checking modules can load:")
	local modules = {
		"setup.ai",
		"setup.backup",
		"setup.claude",
		"setup.codespace",
		"setup.extras",
		"setup.git",
		"setup.nvim",
		"setup.shell",
		"setup.setup",
	}

	local all_ok = true
	for _, name in ipairs(modules) do
		local ok, mod = pcall(require, name)
		if ok then
			print(string.format("  ✓ %s", name))
		else
			print(string.format("  ✗ %s: %s", name, tostring(mod)))
			all_ok = false
		end
	end

	print()
	if all_ok then
		print("all modules validated successfully!")
		return 0
	else
		print("validation failed")
		return 1
	end
end

return {
	main = main,
}
