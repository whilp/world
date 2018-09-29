#!/bin/bash

set -euo pipefail

main() {
	command -v python
	bazel="$1"
	shift
	$bazel \
		--output_base=$"HOME/.cache/bazel" \
		--host_jvm_args=-Xmx500m \
		--host_jvm_args=-Xms500m \
		test \
		--config=ci \
		--experimental_repository_cache="$HOME/.bazel_repository_cache" \
		--local_resources=400,1,1.0 \
		"$@"
	return $?
}

main "$@"
exit $?
