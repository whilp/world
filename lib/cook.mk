lib_lua_modules :=
lib_dirs :=
lib_libs :=
lib_tests :=

include lib/aerosnap/cook.mk
include lib/claude/cook.mk
include lib/daemonize/cook.mk
include lib/environ/cook.mk
include lib/spawn/cook.mk
include lib/home/cook.mk
include lib/nvim/cook.mk
include lib/whereami/cook.mk
include lib/work/cook.mk

lib-test: $(lib_tests) ## Run lib module tests

.PHONY: lib-test
