modules += hook
hook_srcs := $(wildcard lib/hook/*.lua)
hook_tests := $(filter lib/hook/test_%.lua,$(hook_srcs))
hook_libs := $(addprefix $(o)/,$(filter-out $(hook_tests),$(hook_srcs)))
hook_files := $(hook_libs)
