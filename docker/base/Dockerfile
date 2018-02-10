FROM alpine:latest

RUN apk add --no-cache \
	curl \
	make \
	musl-dev \
	gcc

ADD bin/get-and-check /usr/bin/get-and-check
ADD bin/shazam /usr/bin/shazam

RUN addgroup -g 8001 build && adduser -u 8001 -G build -s /bin/sh -D -h /build build
WORKDIR /build
