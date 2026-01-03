# lib/build - build tools (fetch, extract, install)

lib_dirs += $(o_any)/build/lib
lib_libs += $(o_any)/build/lib/build/install.lua
lib_libs += $(o_any)/build/lib/build/fetch.lua
lib_libs += $(o_any)/build/lib/build/review.lua

$(o_any)/build/lib/build/%.lua: lib/build/%.lua
	mkdir -p $(@D)
	cp $< $@

# note: test dependencies for lib/build tests are at the end of Makefile
# (after includes, so manifest variables are available)

update-pr: $(lua_bin) ## Update PR title/description from .github/pr/<number>.md
	@$(lua_bin) lib/build/pr.lua || true

.PHONY: update-pr
