#!/bin/sh

exec docker run -ti \
	-v $PWD:/home/me:cached \
	-v ~/src:/src:cached \
	-v cache:/cache:cached \
	-v /var/run/docker.sock:/var/run/docker.sock \
	bazel/image:image
