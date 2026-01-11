modules += tl
tl_version := 3p/tl/version.lua
tl_srcs := $(wildcard 3p/tl/*.lua)
tl_run := $(o)/bin/run-teal.lua
tl_gen_script := $(o)/bin/tl-gen.lua
tl_files := $(tl_run) $(tl_gen_script)
tl_tests := $(wildcard 3p/tl/test_*.lua)
tl_deps := argparse cosmos

.PRECIOUS: $(tl_files)
teal_runner := $(bootstrap_cosmic) -- $(tl_run)

# tl gen: compile .tl files to .lua (using wrapper script)
tl_gen = TL_BIN=$(tl_staged) $(bootstrap_cosmic) -- $(tl_gen_script)
