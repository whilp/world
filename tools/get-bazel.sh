#!/bin/bash

set -euo pipefail

tmp=""

main() {
	os="$1"
	normalized_os=$(normalize_os "$os")
	tmp=$(mktemp -d)

	get "$normalized_os" "$tmp/install.sh"
	check "$os" "$out"
	run "$tmp/install.sh"
}

get() {
	os="$1"
	out="$2"

	version="0.16.0"
	arch="x86_64"
	url="https://github.com/bazelbuild/bazel/releases/download/${version}/bazel-${version}-installer-${os}-${arch}.sh"

	echo "Fetching $url"
	curl -sLo "$out" "$url"
}

check() {
	os="$1"
	out="$2"

	want=""
	case "$os" in
	darwin) want="bb4720c027a991643be7dabee5c96a84c3776d6e2da92bf65df012a7cec15569" ;;
	linux) want="bdef5499ea21baa69e707391b463105b32c4a2fbf1ab045da53f4023c1a033be" ;;
	esac

	got=$(checksum "$out")
	if [ "$want" != "$got" ]; then
		echo "wanted SHA256 $want but got $got"
		return 1
	fi
	return 0
}

checksum() {
	got_fields=$(sha256sum "$out" 2>/dev/null || true)
	if [ -z "$got_fields" ]; then
		got_fields=$(shasum -a 256 "$out" 2>/dev/null || true)
	fi
	got="${got_fields%% *}"
	echo "$got"
	return 0
}

run() {
	install="$1"
	chmod a+x "$install"
	"$install" --user
}

normalize_os() {
	os="$1"
	case "$os" in
	osx) echo darwin ;;
	*) echo "$os" ;;
	esac
}

cleanup() {
	rm -rf "$tmp"
}

trap cleanup EXIT
main "$@"
exit $?
