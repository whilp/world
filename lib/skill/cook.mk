modules += skill
skill_srcs := $(wildcard lib/skill/*.lua)
skill_tests := $(filter lib/skill/test_%.lua,$(skill_srcs))
skill_libs := $(addprefix $(o)/,$(filter-out $(skill_tests),$(skill_srcs)))
skill_files := $(skill_libs)
