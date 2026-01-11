modules += daemonize
daemonize_tl_srcs := $(wildcard lib/daemonize/*.tl)
daemonize_lua_srcs := $(wildcard lib/daemonize/*.lua)
daemonize_srcs := $(daemonize_tl_srcs) $(daemonize_lua_srcs)
daemonize_tests := $(filter lib/daemonize/test%.tl,$(daemonize_tl_srcs))
daemonize_files := o/any/daemonize/lib/daemonize/init.lua

lib_lua_modules += daemonize
lib_dirs += o/any/daemonize/lib

o/any/daemonize/lib/daemonize/init.lua: $(o)/teal/lib/daemonize/init.lua
	mkdir -p $(@D)
	cp $< $@
