#!/bin/bash

set -euo pipefail

clipper_pid=

main () {
    #clipper="$1"
    
    # Listen on localhost:8377; mounting a socket into the container
    # doesn't work. https://github.com/docker/for-mac/issues/483
    #$clipper &
    #clipper_pid=$!

    docker run -ti \
        -v ~/src:/src:cached \
        -v cache:/cache:cached \
        -v /var/run/docker.sock:/var/run/docker.sock \
	"$@" \
        bazel/image:image
    return $?
}

clean () {
    #kill -9 "$clipper_pid"
    true
}
trap clean EXIT

main "$@"
exit $?
