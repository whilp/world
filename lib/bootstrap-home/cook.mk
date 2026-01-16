modules += bootstrap-home
lib_lua_modules += bootstrap-home
lib_dirs += o/lib/bootstrap-home
bootstrap-home_tl_files := lib/bootstrap-home/init.tl
bootstrap-home_deps :=

# Generated shas.lua file (created during release build)
bootstrap-home_shas := o/lib/bootstrap-home/shas.lua

# Bootstrap-home binary (bundled with cosmic + bootstrap-home module)
bootstrap-home_bin := o/bin/bootstrap-home

# Note: bootstrap-home binary and shas file are built by release target
# They depend on artifacts being present, so they can't be part of normal build
# gen_bootstrap_shas tool is defined in lib/build/cook.mk
