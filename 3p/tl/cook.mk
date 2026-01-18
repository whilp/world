modules += tl
tl_version := 3p/tl/version.lua
tl_srcs := $(wildcard 3p/tl/*.lua)
tl_run := $(o)/bin/run-teal.lua
tl_files := $(tl_run)
tl_tests := $(wildcard 3p/tl/test_*.tl)
tl_deps := cosmos

.PRECIOUS: $(tl_files)
teal_runner := $(bootstrap_cosmic) -- $(tl_run)

# tl gen: compile .tl files to .lua (using lib/cosmic/tl-gen.lua as library)
tl_gen = $(bootstrap_cosmic) lib/cosmic/tl-gen.lua --

# run-teal.lua is compiled from .tl using tl_gen
$(tl_run): 3p/tl/run-teal.tl $(types_files) lib/cosmic/tl-gen.lua | $(bootstrap_files)
	@mkdir -p $(@D)
	@$(tl_gen) $< -o $@
