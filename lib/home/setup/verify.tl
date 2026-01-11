require("cosmo") -- luacheck: ignore

local record Module
	main: function(): number
end

local function main(): number
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
		local status, loaded_module = pcall(require, name) as (boolean, {string: any})
		if not status then
			print(string.format("✗ %s: failed to load", name))
			print("  " .. tostring(loaded_module))
			all_ok = false
		else
			print(string.format("✓ %s: loaded successfully", name))

			if type(loaded_module) == "table" then
				local exports: {string} = {}
				for k, v in pairs(loaded_module) do
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

	local status, setup = pcall(require, "setup.setup") as (boolean, Module)
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
