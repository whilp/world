#!/bin/bash

set -euo pipefail

this_file="${BASH_SOURCE[0]}"
workspace_status=tools/workspace_status.sh

# usage: $0 stable STABLE_HEAD
main() {
	if [ -x "$workspace_status" ]; then
		from_workspace_status "$@"
	else
		from_self "$@"
	fi
	return $?
}

from_workspace_status() {
	stable_or_volatile="$1"
	key="$2"
	"$workspace_status" | sed -ne "s/^${key} //p"
}

from_self() {
	stable_or_volatile="$1"
	key="$2"
	sed -ne "s/^## ${stable_or_volatile} ${key} //p" "$this_file"
}

main "$@"
exit $?
