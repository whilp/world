local cosmo = require("cosmo")

package.path = "/workspaces/dotfiles/.config/setup/?.lua;" .. package.path

local modules = {
	"env",
	"ai",
	"backup",
	"claude",
	"codespace",
	"extras",
	"git",
	"nvim",
	"shell",
}

print("verifying setup modules...")
print()

for _, name in ipairs(modules) do
	local status, module = pcall(require, name)
	if not status then
		print(string.format("✗ %s: failed to load", name))
		print("  " .. tostring(module))
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

local status, setup = pcall(require, "setup")
if not status then
	print("✗ setup.lua: failed to load")
	print("  " .. tostring(setup))
	os.exit(1)
else
	print("✓ setup.lua: loaded successfully")
	if setup.main and type(setup.main) == "function" then
		print("  exports: main (function)")
	end
end

print()
print("all modules verified!")
