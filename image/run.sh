#!/bin/bash

set -euo pipefail

main() {
	docker run -P \
		-v ~/src:/src:cached \
		-v cache:/cache:cached \
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
