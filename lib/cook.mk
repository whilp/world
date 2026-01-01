lib_lua_modules :=
lib_dirs :=
lib_libs :=
lib_tests :=

include lib/spawn/cook.mk

lib-test: $(lib_tests) ## Run lib module tests

.PHONY: lib-test
