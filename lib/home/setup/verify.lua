-- teal ignore: type annotations needed
require("cosmo") -- luacheck: ignore

local function main()
	local modules = {
		"setup.env",
		"setup.ai",
		"setup.backup",
		"setup.claude",
		"setup.codespace",
		"setup.extras",
		"setup.git",
		"setup.nvim",
		"setup.shell",
	}

	print("verifying setup modules...")
	print()

	local all_ok = true
	for _, name in ipairs(modules) do
		local status, module = pcall(require, name)
		if not status then
			print(string.format("✗ %s: failed to load", name))
			print("  " .. tostring(module))
			all_ok = false
		else
			print(string.format("✓ %s: loaded successfully", name))

			if type(module) == "table" then
				local exports = {}
				for k, v in pairs(module) do
					table.insert(exports, string.format("%s (%s)", k, type(v)))
				end
				table.sort(exports)
				if #exports > 0 then
					print("  exports: " .. table.concat(exports, ", "))
				end
			end
		end
	end

	print()

	local status, setup = pcall(require, "setup.setup")
	if not status then
		print("✗ setup.setup: failed to load")
		print("  " .. tostring(setup))
		all_ok = false
	else
		print("✓ setup.setup: loaded successfully")
		if setup.main and type(setup.main) == "function" then
			print("  exports: main (function)")
		end
	end

	print()
	if all_ok then
		print("all modules verified!")
		return 0
	else
		print("verification failed")
		return 1
	end
end

return {
	main = main,
}
