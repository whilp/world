#!/bin/bash
# verify transitive dependency tracking works

set -e

echo "=== verifying transitive dependency tracking ==="
echo

# test 1: same-module dependency
echo "test 1: same-module dependency"
echo "-------------------------------"
echo "touch lib/build/review.lua (dependency of test_review.lua)"
touch lib/build/review.lua

echo "check if test needs rebuild:"
if make -n o/luatest/lib/build/test_review.lua.ok 2>&1 | grep -q "lib/build/test_review.lua"; then
  echo "✓ test will rebuild (dependency tracked)"
else
  echo "✗ test won't rebuild (dependency NOT tracked)"
fi
echo

# test 2: cross-module dependency
echo "test 2: cross-module dependency"
echo "--------------------------------"
echo "touch lib/cosmic/spawn.lua (used by lib/build/test_luacheck.lua)"
touch lib/cosmic/spawn.lua

echo "check if test needs rebuild:"
if make -n o/luatest/lib/build/test_luacheck.lua.ok 2>&1 | grep -q "lib/build/test_luacheck.lua"; then
  echo "✓ test will rebuild (cross-module dependency tracked)"
else
  echo "✗ test won't rebuild (cross-module dependency NOT tracked)"
fi
echo

# test 3: transitive via TEST_ARGS
echo "test 3: transitive via file arguments"
echo "--------------------------------------"
echo "touch lib/build/manifest.lua (used by test_luafiles.lua via execution)"
touch lib/build/manifest.lua

echo "check if test needs rebuild:"
if make -n o/luatest/lib/build/test_luafiles.lua.ok 2>&1 | grep -q "lib/build/test_luafiles.lua"; then
  echo "✓ test will rebuild (file dependency tracked)"
else
  echo "✗ test won't rebuild (file dependency NOT tracked)"
fi
echo

# summary
echo "=== summary ==="
echo "to fix missing dependencies, apply doc/transitive-deps.patch"
echo "or add the rules from doc/transitive-deps-solution.md"
