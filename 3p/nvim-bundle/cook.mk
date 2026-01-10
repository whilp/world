# Pre-built nvim bundle fetched from nvim-release workflow
# This provides nvim_dir for modules that need the full bundle
modules += nvim-bundle
nvim-bundle_version := 3p/nvim-bundle/version.lua
nvim-bundle_tests := 3p/nvim/test_nvim.lua 3p/nvim/test_treesitter.lua 3p/nvim/test_packpath.lua

# Override nvim_dir to use pre-built bundle instead of local build
# Modules depending on nvim should now depend on nvim-bundle
nvim_dir := $(nvim-bundle_staged)
