FROM debian:unstable-slim

ENV USER me
ENV HOME /home/me
ENV USER_ID 9001
ENV DEBIAN_FRONTEND noninteractive
ENV LC_ALL C.UTF-8

RUN mkdir /usr/share/man/man1 \
	&& apt-get update \
	&& apt-get install --no-install-recommends -yq \
	apt-utils \
	bash \
	ca-certificates \
	coreutils \
	curl \
	curl \
	entr \
	file \
	fish \
	g++-multilib \
	gcc \
	git \
	golang \
	golang-golang-x-tools \
	iptables \
	ldnsutils \
	less \
	libapparmor1 \
	libc6-dev \
	libdevmapper1.02.1 \
	libltdl7 \
	locales \
	make \
	man \
	musl-dev \
	musl-tools \
	openjdk-8-jdk \
	pkg-config \
	python3 \
	python3-pip \
	python3-distutils \
	signify-openbsd \
	tmux \
	unzip \
	zip \
	zlib1g-dev

RUN echo "\$LC_ALL UTF-8" > /etc/locale.gen \
	&& locale-gen $LC_ALL \
	&& dpkg-reconfigure locales \
	&& /usr/sbin/update-locale LANG=$LC_ALL

ADD bin/get-and-check /usr/bin/get-and-check
ADD bin/shazam /usr/bin/shazam

RUN get-and-check \
	https://download.docker.com/linux/debian/dists/buster/pool/edge/amd64/docker-ce_17.12.0~ce-0~debian_amd64.deb \
	/tmp/docker.deb \
	18a1b878a7b4856257ba9367b760be1f69abde450e4c580593297cf0bc2e837f \
	&& dpkg -i /tmp/docker.deb

RUN get-and-check \
	https://github.com/bazelbuild/bazel/releases/download/0.14.0/bazel-0.14.0-installer-linux-x86_64.sh \
	/tmp/bazel.sh \
	34488cd55f95668cfad45874d9d58a220ea8233beede60ade1b036ebc661c1d0 \
	&& bash /tmp/bazel.sh \
	&& bazel version

RUN ln -s /bin/python3 /bin/python \
	&& ln -s /bin/pip3 /bin/pip

ADD . /home/me
WORKDIR /home/me

#ENTRYPOINT ["/home/me/docker/home/entrypoint"]
