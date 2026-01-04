modules += checker
checker_srcs := $(wildcard lib/checker/*.lua)
checker_tests := $(filter lib/checker/test_%.lua,$(checker_srcs))
checker_files := $(addprefix $(o)/,$(filter-out $(checker_tests),$(checker_srcs)))
