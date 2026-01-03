modules += cosmic
cosmic_files := $(addprefix $(o)/lib/cosmic/,init.lua spawn.lua walk.lua help.lua)
cosmic_tests := $(wildcard lib/cosmic/test_*.lua)
