#!/bin/bash

set -euo pipefail

tmp=""

main() {
	target="$1"
	url=$(./tools/get-status.sh stable STABLE_BAZEL_URL)
	sha256=$(./tools/get-status.sh stable STABLE_BAZEL_SHA256)

	tmp=$(mktemp -d)
	curl -sLo "$tmp/bazel" "$url"
	verify_sha256 "$tmp/bazel" "$sha256"
	chmod a+x "$tmp/bazel"
	mv "$tmp/bazel" "$target"
}

verify_sha256() {
	f="$1"
	want="$2"
	got=$(checksum "$f")
	if [ "$want" != "$got" ]; then
		echo "wanted SHA256 $want but got $got"
		return 1
	fi
	return 0
}

checksum() {
	out="$1"
	got_fields=$(sha256sum "$out" 2>/dev/null || true)
	if [ -z "$got_fields" ]; then
		got_fields=$(shasum -a 256 "$out" 2>/dev/null || true)
	fi
	got="${got_fields%% *}"
	echo "$got"
	return 0
}

cleanup() {
	rm -rf "$tmp"
}

trap cleanup EXIT
main "$@"
exit $?
