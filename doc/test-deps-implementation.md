# test dependency implementation plan

## current state

we have ~20 explicit test dependency lines like:
```make
$(luatest_o)/lib/build/test_review.lua.ok: $(o_any)/build/lib/build/review.lua
$(luatest_o)/lib/build/test_luafiles.lua.ok: $(manifest_git) $(manifest_luafiles) $(manifest_luatests)
$(luatest_o)/lib/cosmic/test_cosmic.lua.ok: $(lib_libs)
```

## proposed implementation (hybrid approach)

### phase 1: convention-based (simple, covers 70% of cases)

add to Makefile:
```make
# convention: tests in lib/foo/ depend on all lib/foo/*.lua (excluding test_*.lua)
# this handles the common case automatically
define lib_test_convention
$(luatest_o)/lib/$(1)/test%.lua.ok: lib/$(1)/*.lua
endef

# apply to all lib modules
lib_modules := $(shell ls -d lib/*/ | xargs -n1 basename)
$(foreach mod,$(lib_modules),$(eval $(call lib_test_convention,$(mod))))
```

this would automatically make `lib/build/test_review.lua.ok` depend on all `lib/build/*.lua` files.

**pros**: zero config, works immediately
**cons**: over-approximates (depends on files it might not use)

### phase 2: explicit deps via function (opt-in for complex cases)

modify test runner to support:
```lua
-- in test file: lib/build/test_luafiles.lua
function test_dependencies()
  return {
    files = {
      "lib/build/manifest.lua",
    },
    make_vars = {
      "manifest_git",
      "manifest_luafiles",
      "manifest_luatests",
    },
  }
end
```

runner generates `o/luatest/lib/build/test_luafiles.lua.deps`:
```make
o/luatest/lib/build/test_luafiles.lua.ok: lib/build/manifest.lua
o/luatest/lib/build/test_luafiles.lua.ok: $(manifest_git)
o/luatest/lib/build/test_luafiles.lua.ok: $(manifest_luafiles)
o/luatest/lib/build/test_luafiles.lua.ok: $(manifest_luatests)
```

Makefile includes:
```make
# include auto-generated dependency files
deps_files := $(patsubst %.ok,%.deps,$(luatest_files))
-include $(deps_files)

# rule to generate .deps files
$(luatest_o)/%.deps: % $(luatest_script)
	$(lua_bin) $(luatest_script) --extract-deps $< $@
```

### phase 3: annotation-based (even simpler alternative)

```lua
--test-depends: lib/build/manifest.lua
--test-depends-var: manifest_git
--test-depends-var: manifest_luafiles
```

extract with simple awk/sed:
```make
$(luatest_o)/%.deps: %
	@mkdir -p $(@D)
	@grep '^--test-depends:' $< | sed 's/^--test-depends: */$(luatest_o)\/$*.ok: /' > $@
	@grep '^--test-depends-var:' $< | sed 's/^--test-depends-var: */$(luatest_o)\/$*.ok: $$(/;s/$$/)/' >> $@
```

## recommendation

start with **annotation-based** (phase 3):
- simplest to implement (no runner changes)
- explicit and greppable
- no bootstrap issues
- can add function-based later if needed

implementation checklist:
- [ ] add .deps generation rule to Makefile
- [ ] add -include for .deps files
- [ ] add annotations to existing tests with explicit deps
- [ ] remove explicit deps from cook.mk files
- [ ] update test template/documentation

## comparison with C dependency tracking

C compilers use `gcc -MD -MF foo.d` to generate:
```make
foo.o: foo.c foo.h bar.h
```

our approach is similar but simpler:
- C: compiler tracks what headers got included (runtime discovery)
- us: test author declares deps (explicit annotation)

trade-off: less automatic but simpler, more predictable

## alternative: keep current approach

current explicit rules in cook.mk work fine for:
- small number of special cases (~20 lines)
- dependencies are stable
- cook.mk is the right place for build metadata

**counter-argument for automation:**
- deps live next to test code (locality)
- less chance of getting out of sync
- test author knows best what they depend on
- scales better with more tests

## key question

is the complexity of .deps generation worth it for ~20 dependency lines?

**yes if:**
- we expect many more tests with custom deps
- we want deps co-located with test code
- we want automated tracking

**no if:**
- current approach is working fine
- ~20 lines of make is manageable
- explicit is better than implicit
