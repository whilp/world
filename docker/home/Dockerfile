FROM debian:unstable-slim

ENV USER me
ENV HOME /home/me
ENV USER_ID 9001

RUN mkdir /usr/share/man/man1 \
	&& apt-get update \
	&& DEBIAN_FRONTEND=noninteractive apt-get install --no-install-recommends -yq \
	apt-utils \
	git \
	make \
	python3 \
	python3-pip \
	curl \
	file \
	fish \
	tmux \
	golang \
	bash \
	coreutils \
	golang-golang-x-tools \
	man \
	signify-openbsd \
	musl-dev \
	musl-tools \
	libc6-dev \
	gcc \
	ca-certificates \
	curl \
	iptables \
	libapparmor1 \
	libdevmapper1.02.1 \
	libltdl7 \
	unzip \
	openjdk-8-jdk \
	g++-multilib \
	less \
	ldnsutils

ADD bin/get-and-check /usr/bin/get-and-check
ADD bin/shazam /usr/bin/shazam

RUN get-and-check \
	https://download.docker.com/linux/debian/dists/buster/pool/edge/amd64/docker-ce_17.12.0~ce-0~debian_amd64.deb \
	/tmp/docker.deb \
	18a1b878a7b4856257ba9367b760be1f69abde450e4c580593297cf0bc2e837f \
	&& dpkg -i /tmp/docker.deb

RUN get-and-check \
	https://github.com/bazelbuild/bazel/releases/download/0.10.0/bazel-0.10.0-without-jdk-installer-linux-x86_64.sh \
	/tmp/bazel.sh \
	ef4b224be7c620825a023c22b02144f39a62e7222e4825582ef3d79261d9d60a \
	&& bash /tmp/bazel.sh \
	&& bazel version

ADD . /home/me
WORKDIR /home/me

#ENTRYPOINT ["/home/me/docker/home/entrypoint"]
