#!/bin/bash

set -euo pipefail

env

main () {
	out="$1"
	HEAD=$(sed -ne 's/STABLE_HEAD //p' bazel-out/stable-status.txt) 
	git archive --prefix /home/me $HEAD -o "$out"
}

main "$@"
exit $?
