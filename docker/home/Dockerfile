FROM debian:unstable-slim

ENV USER me
ENV HOME /home/me
ENV USER_ID 9001

RUN apt-get update &&\
	DEBIAN_FRONTEND=noninteractive apt-get install --no-install-recommends -yq \
	apt-utils \
	git \
	make \
	python3 \
	docker \
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
	libltdl7

RUN curl -sLo /tmp/docker.deb \
	https://download.docker.com/linux/debian/dists/buster/pool/edge/amd64/docker-ce_17.12.0~ce-0~debian_amd64.deb && \
	dpkg -i /tmp/docker.deb

ADD . /home/me
WORKDIR /home/me

#ENTRYPOINT ["/home/me/docker/home/entrypoint"]
