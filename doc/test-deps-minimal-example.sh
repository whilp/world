#!/bin/bash
# minimal working example of test dependency tracking

set -e

# create example test with annotations
cat > /tmp/example_test.lua << 'EOF'
--test-depends: lib/foo.lua
--test-depends: lib/bar.lua
--test-depends-var: some_make_variable

local lu = require("luaunit")

function test_something()
  lu.assertEquals(1, 1)
end
EOF

# generate .deps file from annotations
generate_deps() {
  local test_file=$1
  local ok_target=$2
  local deps_file=$3

  # extract file dependencies
  grep '^--test-depends:' "$test_file" | \
    sed "s|^--test-depends: *|$ok_target: |" > "$deps_file"

  # extract make variable dependencies
  grep '^--test-depends-var:' "$test_file" | \
    sed "s|^--test-depends-var: *|$ok_target: \$(|; s|$|)|" >> "$deps_file"
}

# generate deps
generate_deps /tmp/example_test.lua \
  'o/luatest/example_test.lua.ok' \
  /tmp/example_test.lua.deps

echo "generated .deps file:"
cat /tmp/example_test.lua.deps
echo

# in Makefile, you would do:
cat << 'EOF'
# in Makefile:

# rule to generate .deps for each test
$(luatest_o)/%.deps: %
	@mkdir -p $(@D)
	@grep '^--test-depends:' $< | sed 's/^--test-depends: */$(luatest_o)\/$*.ok: /' > $@ || touch $@
	@grep '^--test-depends-var:' $< | sed 's/^--test-depends-var: */$(luatest_o)\/$*.ok: $$(/;s/$$/)/' >> $@ || true

# include all .deps files
-include $(patsubst %.ok,%.deps,$(luatest_files))

# .deps files are created as side effect of test execution
$(luatest_o)/%.ok: % $(luatest_script) $(luatest_o)/%.deps
	$(TEST_ENV) $(luatest_runner) $< $@ $(TEST_ARGS)
EOF
