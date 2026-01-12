modules += daemonize
daemonize_tl_srcs := $(wildcard lib/daemonize/*.tl)
daemonize_lua_srcs := $(wildcard lib/daemonize/*.lua)
daemonize_srcs := $(daemonize_tl_srcs) $(daemonize_lua_srcs)
daemonize_tests := $(filter lib/daemonize/test%.tl,$(daemonize_tl_srcs))
daemonize_files := $(patsubst lib/%.tl,o/any/lib/%.lua,$(filter-out %test.tl,$(daemonize_tl_srcs)))

lib_lua_modules += daemonize
