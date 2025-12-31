lua_libs += lfs
lib_dirs += o/%/lfs/lib
libs += o/%/lfs/lib/lfs.lua

o/%/lfs/lib/lfs.lua: 3p/lua/lfs_stub.lua
	mkdir -p $(dir $@)
	cp $< $@
