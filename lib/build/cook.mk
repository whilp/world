modules += build
build_fetch := $(o)/bin/build-fetch.lua
build_stage := $(o)/bin/build-stage.lua
build_check_report := $(o)/bin/check-report.lua
build_files := $(build_fetch) $(build_stage) $(build_check_report)

.PRECIOUS: $(build_files)
check_reporter := $(bootstrap_cosmic) $(build_check_report)
