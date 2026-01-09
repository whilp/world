modules += skill
skill_srcs := $(wildcard lib/skill/*.lua)
skill_tests := $(filter lib/skill/test_%.lua,$(skill_srcs))
skill_libs := $(addprefix $(o)/,$(filter-out $(skill_tests),$(skill_srcs)))

skill_hook_srcs := $(wildcard lib/skill/hook/*.lua)
skill_hook_tests := $(filter lib/skill/hook/test_%.lua,$(skill_hook_srcs))
skill_hook_libs := $(addprefix $(o)/,$(filter-out $(skill_hook_tests),$(skill_hook_srcs)))

skill_tests += $(skill_hook_tests)
skill_files := $(skill_libs) $(skill_hook_libs)
