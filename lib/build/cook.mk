modules += build
build_fetch := $(o)/bin/build-fetch.lua
build_stage := $(o)/bin/build-stage.lua
build_check_report := $(o)/bin/check-report.lua
build_check_update := $(o)/bin/check-update.lua
build_reporter := $(o)/bin/reporter.lua
build_files := $(build_fetch) $(build_stage) $(build_check_report) $(build_check_update) $(build_reporter)

.PRECIOUS: $(build_files)
reporter := $(bootstrap_cosmic) $(build_reporter)
check_reporter := $(bootstrap_cosmic) $(build_check_report)
update_runner := $(bootstrap_cosmic) $(build_check_update)
