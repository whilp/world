#!/bin/bash

set -euo pipefail

main() {
	cid=$(run "$@")
	port=$(docker port "$cid" 22)
	echo "ssh://root@localhost:${port##*:}"
}

run() {
	docker run -dP \
		-v src:/src:cached \
		-v cache:/cache:cached \
		-v secrets:/secrets:cached \
		-v /var/run/docker.sock:/var/run/docker.sock \
		"$@" \
		bazel/image:image
	return $?
}

clean() {
	true
}

trap clean EXIT

main "$@"
exit $?
