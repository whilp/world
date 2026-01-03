#!/bin/bash
# test if .ok files depend on their test files

set -e

TEST_FILE="lib/build/test_review.lua"
OK_FILE="o/luatest/lib/build/test_review.lua.ok"

echo "=== Testing if .ok files depend on test files ==="
echo

# clean and build
echo "1. Building fresh test..."
rm -f "$OK_FILE"
make "$OK_FILE" >/dev/null 2>&1
echo "   Built: $OK_FILE"
ls -l "$OK_FILE" | awk '{print "   Timestamp:", $6, $7, $8}'
TIMESTAMP1=$(stat -c %Y "$OK_FILE")
echo

# wait and touch test file
sleep 2
echo "2. Touching test file: $TEST_FILE"
touch "$TEST_FILE"
echo

# rebuild
echo "3. Running make again..."
if make "$OK_FILE" 2>&1 | grep -q "lib/build/test_review.lua"; then
    echo "   ✓ Test ran (rebuild triggered)"
else
    echo "   ✗ Test did not run (rebuild NOT triggered)"
fi
TIMESTAMP2=$(stat -c %Y "$OK_FILE")
echo

# compare timestamps
echo "4. Checking .ok file timestamp..."
if [ "$TIMESTAMP2" -gt "$TIMESTAMP1" ]; then
    echo "   ✓ .ok file was updated (newer timestamp)"
    echo "   Before: $TIMESTAMP1"
    echo "   After:  $TIMESTAMP2"
else
    echo "   ✗ .ok file was NOT updated (same timestamp)"
    echo "   Before: $TIMESTAMP1"
    echo "   After:  $TIMESTAMP2"
fi
echo

# check the rule
echo "5. Checking makefile rule..."
echo "   Base rule: \$(luatest_o)/%.ok: % ..."
echo "   The '%' matches the test file itself"
echo "   So $OK_FILE should depend on $TEST_FILE"
echo

# verify with make
echo "6. Verifying with make --print-data-base..."
if make --print-data-base 2>/dev/null | grep -A3 "^$OK_FILE:" | grep -q "$TEST_FILE"; then
    echo "   ✓ Dependency found in make database"
else
    echo "   ? Dependency not clearly visible (might be pattern rule)"
fi
