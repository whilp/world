FROM build:base

ENV VERSION 36b573a
ENV CHECKSUM 92f99466bda833b6e5b4cc6ba8a3e3035de94653c476614c85482c96aefc7d11

# https://github.com/martanne/vis/blob/5963289d0257c7bd422c3cf54866a846168f5c7a/Dockerfile
RUN apk add --no-cache \
	musl-dev \
	fortify-headers \
	gcc \
	make \
	libtermkey-dev \
	ncurses-dev \
	ncurses-static \
	lua5.3-dev \
	lua5.3-lpeg \
	lua-lpeg-dev \
	acl-dev \
	xz-dev \
	tar \
	xz \
	ca-certificates

RUN sed -i 's/Libs: /Libs: -L${INSTALL_CMOD} /' /usr/lib/pkgconfig/lua5.3.pc
RUN mv /usr/lib/lua/5.3/lpeg.a /usr/lib/lua/5.3/liblpeg.a
RUN sed -i 's/-ltermkey/-ltermkey -lunibilium/' /usr/lib/pkgconfig/termkey.pc
# TODO contribute a proper libuntar package to Alpine
RUN curl -sLo libuntar.tar.gz https://github.com/martanne/libuntar/tarball/3f5e915ad8e6c5faa8dc6b34532e32b519f278f3 \
	&& tar xf libuntar.tar.gz && cd *-libuntar-* \
	&& make \
	&& mkdir -p /usr/local/include \
	&& cp lib/libuntar.h /usr/local/include \
	&& cp lib/libuntar.a /usr/local/lib

USER build
RUN get-and-check \
	https://github.com/martanne/vis/archive/${VERSION}.tar.gz \
	tgz \
	${CHECKSUM}
RUN mkdir -p bin src \
	&& tar xzf tgz --strip-components=1 -C src
RUN cd src \
	&& ./configure CC='cc --static' \
	&& make clean vis-single vis-menu vis-digraph \
	&& cp vis-single /build/bin/vis \
	&& cp vis-clipboard vis-complete vis-open vis-menu vis-digraph /build/bin/.

